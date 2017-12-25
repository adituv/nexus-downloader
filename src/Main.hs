{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Main where

import           Control.Exception     (bracket_)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8S
import           Data.Coerce           (coerce)
import           Data.IORef
import           Data.Semigroup        ((<>))
import           Data.String           (IsString (..))
import           Data.Time.Clock       (addUTCTime, getCurrentTime)
import           Lens.Micro
import           Network.HTTP.Client   (insertCheckedCookie)
import           Network.HTTP.Conduit  (Cookie (..), CookieJar (..),
                                        Request (..), destroyCookieJar)
import           Network.HTTP.Simple
import           System.IO             (hFlush, hGetEcho, hSetEcho, stdin,
                                        stdout)

data Env = Env
  { _baseurl :: ByteString
  , _game    :: ByteString
  , _gameid  :: Int
  , _nmmVer  :: ByteString
  , _cookies :: IORef CookieJar
  }

type Params = [(ByteString, Maybe ByteString)]

newtype Maybe' a = Maybe' (Maybe a) deriving Show

instance forall a. FromJSON a => FromJSON (Maybe' a) where
  parseJSON v = do
    case fromJSON v :: Result a of
      Success a -> pure $ Maybe' (Just a)
      _         -> pure $ Maybe' (Nothing)

mkDefaultEnv :: IO Env
mkDefaultEnv = do
  let _baseurl = "www.nexusmods.com"
  let _game = "skyrim"
  let _gameid = 110
  let _nmmVer = "0.63.17"
  _cookies <- newIORef mempty
  pure Env{..}

data ModFile = ModFile
  { _name    :: String
  , _modId   :: Int
  , _fileId  :: Int
  , _ownerId :: Int
  , _uri     :: String
  , _size    :: Int
  , _version :: String
  } deriving Show

instance FromJSON ModFile where
  parseJSON = withObject "ModFile" $
    \v ->  ModFile
       <$> v .: "name"
       <*> v .: "mod_id"
       <*> v .: "id"
       <*> v .: "owner_id"
       <*> v .: "uri"
       <*> (read <$> v .: "size")
       <*> v .: "version"

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
              , cookie_secure_only = True
              , cookie_http_only = True
              }

mkRequest :: (MonadIO m, MonadReader Env m) => ByteString -> Params -> m Request
mkRequest endpoint params = do
  Env{..} <- ask
  jar <- liftIO $ readIORef _cookies
  let path = B.concat ["/", _game, "/", endpoint]
  let userAgent = "Nexus Client v" `mappend` _nmmVer
  let req = defaultRequest
              {host=_baseurl, port=443, path, secure=True, cookieJar=Just jar}
              & setRequestHeader "User-Agent" [userAgent]
              & setRequestQueryString params
  pure req

login :: (MonadReader Env m, MonadIO m) => ByteString -> ByteString -> m Bool
login user pass = do
  Env{..} <- ask
  let params = [ ("Login", Nothing)
               , ("username", Just user)
               , ("password", Just pass)
               ]
  req <- mkRequest "Sessions" params
  response <- liftIO $ httpJSON @_ @(Maybe' String) req
  let token = coerce (getResponseBody response) :: Maybe String
  case token of
    Nothing -> do
      liftIO $ putStrLn "Login failed."
      pure False
    Just tok -> do
      liftIO $ putStrLn "Login succeeded!"
      sidCookie <- mkCookie "sid" (fromString tok)
      liftIO $ modifyIORef _cookies
        (\jar -> insertCheckedCookie sidCookie jar True)
      pure True

getModInfo :: (MonadReader Env m, MonadIO m) => ByteString -> m [ModFile]
getModInfo modId = do
  Env{_gameid} <- ask
  req <- mkRequest ("Files/indexfrommod/" <> modId) [("game_id", Just (showBS _gameid))]
  response <- liftIO $ httpJSON req
  pure $ getResponseBody response

main :: IO ()
main = do
  env <- mkDefaultEnv
  flip runReaderT env $ do
    Env{..} <- ask
    user <- liftIO $ runWithLabel "Username" B.getLine
    pass <- liftIO getPassword
    loggedin <- login user pass
    if loggedin then do
      modid <- liftIO $ runWithLabel "ModId" B.getLine
      modInfo <- getModInfo modid
      case modInfo of
        [] -> liftIO $ putStrLn "No files found"
        xs@(x:_) -> liftIO $ do
          putStrLn $ "Mod: " <> _name x
          putStrLn $ "Number of files: " <> show (length xs)
          putStrLn $ "First file download: " <> _uri x
    else
      pure ()
