{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module Nexus.API
  ( module Nexus.Types
  , NexusAPI(..)
  , Params
  -- * API Requests
  , request
  , getGameId
  , getModInfo
  , getModFiles
  , getDownloadInfo
  -- * Session handling
  , login
  , validate
  , userIsPremium
  ) where

import           Nexus.Types

import           Extra

import           Control.Monad.Fail     as CMF
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as C
import           Data.IORef
import           Data.Maybe             (isJust)
import           Data.Semigroup         ((<>))
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Lens.Micro.Platform
import           Network.HTTP.Client    (insertCheckedCookie)
import           Network.HTTP.Conduit   (Cookie (..), Request (..),
                                         Response (..))
import           Network.HTTP.Simple

newtype NexusAPI a = NexusAPI {
    runNexus :: (forall m. (MonadReader NexusEnv m, MonadFail m, MonadIO m) => m a) -- TODO: rename
  }

instance Functor NexusAPI where
  fmap f (NexusAPI inner) = NexusAPI $ fmap f inner

instance Applicative NexusAPI where
  pure x = NexusAPI $ pure x
  NexusAPI f <*> NexusAPI x = NexusAPI $ f <*> x

instance Monad NexusAPI where
  return = pure
  NexusAPI x >>= f = NexusAPI $ x >>= runNexus . f

instance MonadIO NexusAPI where
  liftIO action = NexusAPI $ liftIO action

instance MonadReader NexusEnv NexusAPI where
  ask = NexusAPI ask
  reader f = NexusAPI $ reader f
  local f (NexusAPI x) = NexusAPI $ local f x

instance MonadFail NexusAPI where
  fail str = NexusAPI $ CMF.fail str





type Params = [(ByteString, Maybe ByteString)]

-- | Make a raw Nexus API request from the endpoint and query parameters
request :: (FromJSON a) => ByteString -> Params -> NexusAPI a
request endpoint params = NexusAPI $ do
  env <- ask
  jar <- liftIO . readIORef $ env ^. cookies
  let userAgent = "Nexus Client v" <> env ^. nmmVer
  let req = defaultRequest
                { host=env^.baseurl
                , port=443
                , path= "/" <> env ^. game <> "/" <> endpoint
                , secure=True
                , cookieJar=Just jar
                }
              & setRequestHeader "User-Agent" [userAgent]
              & setRequestQueryString params
  response <- withUpdateCookies $ httpJSON req
  pure $ getResponseBody response

-- | Get the numeric id of the current game
getGameId :: NexusAPI Int
getGameId = NexusAPI $ view gameid

-- | Fetch information on the mod with the given id
getModInfo :: Int -> NexusAPI ModInfo
getModInfo modId = do
  gid <- getGameId
  request ("Mods/" <> showBS modId) [("game_id", Just $ showBS gid)]

-- | Fetch a list of files associated with the mod with the given id
getModFiles :: Int -> NexusAPI [ModFile]
getModFiles modId = do
  gid <- getGameId
  request ("Files/indexfrommod/" <> showBS modId) [("game_id", Just $ showBS gid)]

-- | Fetch a list of possible downloads for the file with the given id
getDownloadInfo :: Int -> NexusAPI [FileDownloadInfo]
getDownloadInfo fileId = do
  gid <- getGameId
  request ("Files/download/" <> showBS fileId) [("game_id", Just $ showBS gid)]

-- * Session handling

-- | Attempts to log into the Nexus using the given username and password.
--   Returns @False@ if the login failed; sets a session cookie and returns
--   @True@ if the login succeeds.
login :: String -> String -> NexusAPI Bool
login user pass = do
  -- TODO: url-encoding user and pass
  token <- request "Sessions" [("Login", Nothing), ("username", Just $ C.pack user), ("password", Just $ C.pack pass)]
  case token of  -- token :: Maybe String
    Nothing -> pure False
    Just tok -> do
      setCookie "sid" (C.pack tok)
      pure True

-- | Check whether the current session cookie is still valid
validate :: NexusAPI Bool
validate = do
  token <- request @(Maybe String) "Sessions" [("Validate", Nothing)]
  pure $ isJust token

-- | Get whether the user's account status has access to more connections and
--   concurrent downloads than default.  This part of the Nexus API is not
--   documented, and the account credential codes are entirely unknown.
userIsPremium :: NexusAPI Bool
userIsPremium = do
  (_:n:_) <- request @([String]) "Core/Libs/Flamework/Entities/User" [("GetCredentials", Nothing)]
  pure $ read @Int n `elem` [4,6,13,27,31,32]

-- * Internal

withUpdateCookies :: (MonadReader NexusEnv m, MonadIO m) => IO (Response a) -> m (Response a)
withUpdateCookies action = do
  jar <- view cookies
  resp <- liftIO action
  liftIO $ writeIORef jar (responseCookieJar resp)
  pure resp

setCookie :: ByteString -> ByteString -> NexusAPI ()
setCookie cookie_name value = NexusAPI $ do
  jar <- view cookies
  host <- view baseurl
  now <- liftIO getCurrentTime
  let inAMonth = addUTCTime (30*1440) now
  let cookie = Cookie { cookie_name = cookie_name
                      , cookie_value = value
                      , cookie_expiry_time = inAMonth
                      , cookie_domain = host
                      , cookie_path = "/"
                      , cookie_creation_time = now
                      , cookie_last_access_time = now
                      , cookie_persistent = True
                      , cookie_host_only = True
                      , cookie_secure_only = False
                      , cookie_http_only = True
                      }
  liftIO $ modifyIORef' jar (\j -> insertCheckedCookie cookie j True)
