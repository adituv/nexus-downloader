{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Main where

--import SuperRecord
import           Control.Exception     (bracket_)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8S
import           Data.Coerce           (coerce)
import           Data.IORef
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
  { _friendlyName :: String
  , _modId        :: Int
  , _fileId       :: Int
  } deriving Show

-- * Standard IO Utilities

getUsername :: IO ByteString
getUsername = do
  putStr "Username: "
  hFlush stdout
  B.getLine

getPassword :: IO ByteString
getPassword = do
  putStr "Password: "
  hFlush stdout
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
              & setRequestHeader "UserAgent" [userAgent]
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
    Nothing -> pure False
    Just tok -> do
      sidCookie <- mkCookie "sid" (fromString tok)
      liftIO $ modifyIORef _cookies
        (\jar -> insertCheckedCookie sidCookie jar True)
      pure True

main :: IO ()
main = do
  env <- mkDefaultEnv
  flip runReaderT env $ do
    Env{..} <- ask
    user <- liftIO getUsername
    pass <- liftIO getPassword
    loggedin <- login user pass
    if loggedin then do
      liftIO $ putStrLn "Login succeeded!"
      allCookies <- liftIO $ destroyCookieJar <$> readIORef _cookies
      let Just sidCookie =  allCookies
                         ^? each
                         .  filtered (\Cookie{..} -> cookie_name == "sid")
      liftIO $ putStr "Token: "
      liftIO $ C8S.putStrLn (cookie_value sidCookie)
    else
      liftIO $ putStrLn "Login failed."
