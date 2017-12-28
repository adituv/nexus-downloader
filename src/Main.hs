{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Main where

import           Conduit
import           Control.Exception            (bracket_)
import           Control.Monad.IfElse
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C8S
import           Data.Char                    (toUpper)
import           Data.IORef
import           Data.Semigroup               ((<>))
import qualified Data.Serialize               as C
import           Data.String                  (IsString (..))
import           Data.Time.Clock              (addUTCTime, getCurrentTime)
import           Extra.Instances              ()
import           GHC.Generics
import           Lens.Micro
import           Network.HTTP.Client          (insertCheckedCookie)
import           Network.HTTP.Conduit         (Cookie (..), CookieJar (..),
                                               Request (..), createCookieJar,
                                               destroyCookieJar, http,
                                               newManager, responseCookieJar,
                                               tlsManagerSettings)
import           Network.HTTP.Simple
import           System.Console.AsciiProgress
import           System.Console.Terminal.Size
import           System.Directory             (doesFileExist)
import           System.Exit                  (exitFailure)
import           System.IO                    (hFlush, hGetEcho, hSetEcho,
                                               stdin, stdout)
import           System.IO.Unsafe             (unsafePerformIO)

data Env = Env
  { _baseurl :: ByteString
  , _game    :: ByteString
  , _gameid  :: Int
  , _nmmVer  :: ByteString
  , _cookies :: IORef CookieJar
  }

instance Show Env where
  show Env{..} = unlines
    [ "Env"
    , "  { _baseurl = " ++ show _baseurl
    , "  , _game = " ++ show _game
    , "  , _gameid = " ++ show _gameid
    , "  , _nmmVer = " ++ show _nmmVer
    , "  , _cookies = IORef (" ++ show cookies ++ ")"
    , "  }"
    ]
    where
      cookies = unsafePerformIO (readIORef _cookies)

instance C.Serialize Env where
  put Env{..} = do
      C.put _baseurl
      C.put _game
      C.put _gameid
      C.put _nmmVer
      C.put (destroyCookieJar cookies)
    where
      cookies = unsafePerformIO (readIORef _cookies)

  get = do
      _baseurl <- C.get
      _game <- C.get
      _gameid <- C.get
      _nmmVer <- C.get
      cookies <- C.get
      let _cookies = unsafePerformIO $ newIORef (createCookieJar cookies)
      pure Env{..}

type Params = [(ByteString, Maybe ByteString)]

mkDefaultEnv :: IO Env
mkDefaultEnv = do
  let _baseurl = "www.nexusmods.com"
  let _game = "skyrim"
  let _gameid = 110
  let _nmmVer = "0.63.17"
  _cookies <- newIORef mempty
  pure Env{..}

configFile :: String
configFile = "nexusdownloader.cfg"

getStoredEnv :: IO Env
getStoredEnv = do
  exists <- doesFileExist configFile
  if exists then do
    file <- B.readFile configFile
    case C.decode file of
      Right env -> pure env
      _         -> mkDefaultEnv
  else
    mkDefaultEnv

data ModInfo = ModInfo
  { _name       :: String
  , _id         :: Int
  , _summary    :: String
  , _version    :: String
  , _author     :: String
  , _lastupdate :: DotNetTime
  , _modPageUri :: String
  } deriving (Generic, Show)

instance FromJSON ModInfo where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = camelTo2 '_' . drop 1
    }

data ModFile = ModFile
  { _name    :: String
  , _id      :: Int
  , _modId   :: Int
  , _ownerId :: Int
  , _uri     :: String
  , _size    :: String
  , _version :: String
  } deriving (Generic, Show)

instance FromJSON ModFile where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = camelTo2 '_' . drop 1
    }

data FileDownloadInfo = FileDownloadInfo
  { _URI            :: String
  , _isPremium      :: Bool
  , _name           :: String
  , _country        :: String
  , _connectedUsers :: Int
  } deriving (Generic, Show)

instance FromJSON FileDownloadInfo where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = (\(~[h],t) -> toUpper h : t) . splitAt 1 . drop 1
    }

showBS :: Show a => a -> ByteString
showBS x = fromString $ show x

-- * Standard IO Utilities

runWithLabel :: MonadIO m => String -> m a -> m a
runWithLabel lab action = do
  liftIO $ putStr (lab ++ ": ")
  liftIO $ hFlush stdout
  action

getPassword :: IO ByteString
getPassword = runWithLabel "Password" $ do
  pass <- withEcho False B.getLine
  putChar '\n'
  return pass

-- | Run an IO action with echoing (displaying what is typed) on stdin turned
--   temporarily on or off, restoring the old value after.
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- * Requests and methods

mkCookie :: (MonadIO m, MonadReader Env m) => ByteString -> ByteString -> m Cookie
mkCookie name value = do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  let inAMonth = addUTCTime (30*1440) now
  pure Cookie { cookie_name = name
              , cookie_value = value
              , cookie_expiry_time = inAMonth
              , cookie_domain = _baseurl
              , cookie_path = "/"
              , cookie_creation_time = now
              , cookie_last_access_time = now
              , cookie_persistent = True
              , cookie_host_only = True
              , cookie_secure_only = False
              , cookie_http_only = True
              }

getCookie :: ByteString -> CookieJar -> Maybe Cookie
getCookie name jar =  destroyCookieJar jar
                   ^? each
                   .  filtered (\Cookie{..} -> cookie_name == name)

mkRequest :: (MonadIO m, MonadReader Env m) => ByteString -> ByteString -> Params -> m Request
mkRequest baseurl endpoint params = do
  Env{..} <- ask
  jar <- liftIO $ readIORef _cookies
  let userAgent = "Nexus Client v" `mappend` _nmmVer
  let req = defaultRequest
              {host=baseurl, port=443, path=endpoint, secure=True, cookieJar=Just jar}
              & setRequestHeader "User-Agent" [userAgent]
              & setRequestQueryString params
  pure req

mkNexusRequest :: (MonadIO m, MonadReader Env m) => ByteString -> Params -> m Request
mkNexusRequest endpoint params = do
  Env{..} <- ask
  mkRequest _baseurl (B.concat ["/", _game, "/", endpoint]) params

-- Also update the cookie jar
httpJSON' :: (MonadIO m, MonadReader Env m, FromJSON a) => Request -> m (Response a)
httpJSON' request = do
  Env{_cookies} <- ask
  response <- httpJSON request
  liftIO $ writeIORef _cookies (responseCookieJar response)
  pure response

download :: Request -> FilePath -> IO ()
download req dest = runConduitRes $ do
    Just (Window _h w) <- liftIO size
    mgr <- liftIO $ newManager tlsManagerSettings
    src <- http req mgr
    let body = getResponseBody src
    let Just (fsize, _) = C8S.readInteger . head . getResponseHeader "Content-Length" $ src
    pg <- liftIO $ newProgressBar def { pgFormat = format, pgTotal = fsize, pgWidth = w }
    body $$+- printProgress pg =$ sinkFile dest
  where
    shorten str
      | length str > 25 = take 22 str ++ "..."
      | otherwise = str
    format = shorten dest <> " [:bar] :percent (:eta s)"

printProgress :: MonadResource m => ProgressBar -> Conduit ByteString m ByteString
printProgress pg = loop
  where
    loop = do
      bytes <- await
      case bytes of
        Nothing -> pure ()
        Just bs -> do
          let len = B.length bs
          liftIO $ tickN pg len
          loop

login :: (MonadReader Env m, MonadIO m) => ByteString -> ByteString -> m Bool
login user pass = do
  Env{..} <- ask
  let params = [ ("Login", Nothing)
               , ("username", Just user)
               , ("password", Just pass)
               ]
  req <- mkNexusRequest "Sessions" params
  response <- httpJSON' @_ @(Maybe String) req
  case getResponseBody response of
    Nothing -> do
      liftIO $ putStrLn "Login failed."
      pure False
    Just token -> do
      liftIO $ putStrLn "Login succeeded!"
      sidCookie <- mkCookie "sid" (fromString token)
      liftIO $ modifyIORef _cookies
        (\jar -> insertCheckedCookie sidCookie jar True)
      pure True

validate :: (MonadReader Env m, MonadIO m) => m Bool
validate = do
  let params = [("Validate", Nothing)]
  req <- mkNexusRequest "Sessions" params
  response <- httpJSON' @_ @(Maybe String) req
  case getResponseBody response of
    Nothing -> pure False
    _       -> pure True

getModInfo :: (MonadReader Env m, MonadIO m) => Int -> m ModInfo
getModInfo modId = do
  Env{_gameid} <- ask
  req <- mkNexusRequest ("Mods/" <> showBS modId) [("game_id", Just (showBS _gameid))]
  response <- httpJSON' req
  pure $ getResponseBody response

getModFiles :: (MonadReader Env m, MonadIO m) => Int -> m [ModFile]
getModFiles modId = do
  Env{_gameid} <- ask
  req <- mkNexusRequest ("Files/indexfrommod/" <> showBS modId) [("game_id", Just (showBS _gameid))]
  response <- httpJSON' req
  pure $ getResponseBody response

getDownloadInfo :: (MonadReader Env m, MonadIO m) => Int -> m [FileDownloadInfo]
getDownloadInfo fileId = do
  Env{_gameid} <- ask
  req <- mkNexusRequest ("Files/download/" <> showBS fileId) [("game_id", Just (showBS _gameid))]
  response <- httpJSON' req
  pure $ getResponseBody response

main :: IO ()
main = do
  env <- getStoredEnv
  flip runReaderT env $ do
    Env{..} <- ask
    unlessM validate $ do
      user <- liftIO $ runWithLabel "Username" B.getLine
      pass <- liftIO getPassword
      unlessM (login user pass) (liftIO exitFailure)
    modid <- liftIO $ runWithLabel "ModId" (read <$> getLine)
    ModInfo{..} <- getModInfo modid
    liftIO $ putStrLn $ "Mod: " <> _name
    liftIO $ putStrLn $ "Summary: " <> _summary
    files <- getModFiles modid
    case files of
      [] -> liftIO $ putStrLn "No mod files." >> exitFailure
      (ModFile{_id=fileid,_uri=filename}:_) -> do
        downloads <- getDownloadInfo fileid
        case downloads of
          [] -> liftIO $ putStrLn "No download locations." >> exitFailure
          (FileDownloadInfo{..}:_) -> do
            req <- parseRequest _URI
            req' <- mkRequest (host req) (path req) []
            let req'' = req' { queryString = queryString req }
            liftIO $ displayConsoleRegions $ download req'' filename
  B.writeFile configFile (C.encode env)
