{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Extra
import qualified Nexus.Types         as Nexus

import           Data.Aeson
import           Data.Serialize
import           Lens.Micro.Platform

data Env = Env
  { _nexusEnv :: Nexus.NexusEnv
  } deriving (Generic, Show)

makeLenses ''Env

instance Serialize Env

mkDefaultEnv :: IO Env
mkDefaultEnv = Env <$> Nexus.mkDefaultEnv

data FileDef = FileDef
  { _filename :: Text
  , _fileid   :: Int
  , _modname  :: Text -- Mod info not strictly required but will be useful for
  , _modid    :: Int  -- reporitng unavailable files to user
  , _optional :: Bool
  } deriving (Generic, Show)

makeLenses ''FileDef

instance FromJSON FileDef where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 1
  }

data Bundle = Bundle
  { _name      :: Text
  , _version   :: Int
  , _updateUrl :: String
  , _files     :: [FileDef]
  } deriving (Generic, Show)

makeLenses ''Bundle

instance FromJSON Bundle where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 1
  }
