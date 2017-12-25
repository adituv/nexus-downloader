{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Main where

--import SuperRecord
import           Control.Exception
import           Control.Monad.Reader
import qualified Data.ByteString.Char8        as C8S
import qualified Data.ByteString.Lazy         as B
import qualified Data.ByteString.Lazy.Char8   as C8
import           Data.IORef
import           Data.List                    (foldl')
import           Data.String
import qualified Data.Text                    as T
import           Data.Time.Clock
import           Lens.Micro
import           Network.HTTP.Client          (Cookie (..), CookieJar (..),
                                               insertCheckedCookie)
import           Network.HTTP.Client.Internal (expose)
import           Network.Wreq
import           System.IO

data Env = Env
  { _baseurl :: String
  , _game    :: String
  , _gameid  :: Int
  , _nmmVer  :: String
  , _cookies :: IORef CookieJar
  }

mkDefaultEnv :: IO Env
mkDefaultEnv = do
  let _baseurl = "https://www.nexusmods.com"
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

getUsername :: IO String
getUsername = do
  putStr "Username: "
  hFlush stdout
  getLine

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

-- The nexus server requires the action parameter to be first while wreq does
-- not guarantee generating the query string in the correct order.  This is a
-- very quick and inefficient workaround
queryString :: String -> [(String, String)] -> String
queryString action params' = concat ["?", action, kvParams]
  where
    kvParams = concat $ concatMap (\(k,v) -> ["&", k, "=", v]) params'

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

request :: String -> String -> String -> String -> [(String, String)] -> IO (Response B.ByteString)
request base game endpoint action params' = do
  let url = concat [base, "/", game, "/", endpoint, queryString action params']
  get url

login :: (MonadIO m, MonadReader Env m) => String -> String -> m Bool
login user pass = do
  let endpoint = "Sessions"
  Env{..} <- ask
  response <- liftIO $ request _baseurl _game endpoint "Login" [("username", user), ("password", pass)]
  let token = response ^. responseBody
  case B.stripSuffix "\"" token >>= B.stripPrefix "\"" of
    Nothing -> pure False
    Just tok' -> do
      now <- liftIO getCurrentTime
      let expires = addUTCTime (30*1440) now
      let cookie = Cookie "sid" (B.toStrict tok') expires (fromString _baseurl) "/" now now True True True True
      liftIO $ modifyIORef' _cookies (\jar -> insertCheckedCookie cookie jar True)
      pure True

validateToken :: B.ByteString -> IO Bool
validateToken token = do
  let queryString = "Validate"
  response <- get ("https://www.nexusmods.com/skyrim/Sessions?" ++ queryString)
  return $ (response ^. responseBody) /= "null"

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
      allCookies <- liftIO $ expose <$> readIORef _cookies
      let Just sidCookie = allCookies ^? each . filtered (\Cookie{..} -> cookie_name == "sid")
      liftIO $ putStr "Token: "
      liftIO $ C8S.putStrLn (sidCookie ^. cookieValue)
    else
      liftIO $ putStrLn "Login failed."
