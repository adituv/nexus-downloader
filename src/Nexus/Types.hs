{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Nexus.Types where

import           Extra.Instances      ()

import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.Char            (toUpper)
import           Data.IORef           (IORef, newIORef, readIORef)
import           Data.Serialize
import           GHC.Generics
import           Lens.Micro.Platform
import           Network.HTTP.Conduit (CookieJar, createCookieJar,
                                       destroyCookieJar)
import           System.IO.Unsafe     (unsafePerformIO)

data NexusEnv = NexusEnv
  { _baseurl :: ByteString
  , _game    :: ByteString
  , _gameid  :: Int
  , _nmmVer  :: ByteString
  , _cookies :: IORef CookieJar
  }

makeLenses ''NexusEnv

instance Show NexusEnv where
  show NexusEnv{..} = unlines
    [ "Env"
    , "  { _baseurl = " ++ show _baseurl
    , "  , _game = " ++ show _game
    , "  , _gameid = " ++ show _gameid
    , "  , _nmmVer = " ++ show _nmmVer
    , "  , _cookies = newIORef " ++ show derefCookies
    , "  }"
    ]
    where
      derefCookies = unsafePerformIO (readIORef _cookies)

instance Serialize NexusEnv where
  put NexusEnv{..} = do
      put _baseurl
      put _game
      put _gameid
      put _nmmVer
      put (destroyCookieJar derefCookies)
    where
      derefCookies = unsafePerformIO (readIORef _cookies)

  get = do
      _baseurl <- get
      _game <- get
      _gameid <- get
      _nmmVer <- get
      derefCookies <- get
      let _cookies = unsafePerformIO $ newIORef (createCookieJar derefCookies)
      pure NexusEnv{..}

-- | Create a default environment.  The default is set up for Skyrim on the
--   nexus, spoofing the current (as of time of development) version of NMM.
mkDefaultEnv :: IO NexusEnv
mkDefaultEnv = do
  let _baseurl = "nexusmods.com"
  let _game = "skyrim"
  let _gameid = 110
  let _nmmVer = "0.63.17"
  _cookies <- newIORef mempty
  pure NexusEnv{..}

-- * API Result Types
-- | Lenses are not provided for these as all API results are very flat
--   structures, and the fields have a lot of name overlap.

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
      fieldLabelModifier = \x -> x & drop 1
                                   & _head %~ toUpper
    }
