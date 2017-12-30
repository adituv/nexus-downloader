{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Extra.Instances() where

import           Data.Serialize       (Serialize (..))
import           Data.Time.Calendar   (Day (..))
import           Data.Time.Clock      (DiffTime, UTCTime (..),
                                       diffTimeToPicoseconds)
import           GHC.Generics         (Generic)
import           Network.HTTP.Conduit (Cookie (..))

instance Serialize Day where
  get = ModifiedJulianDay <$> get
  put = put . toModifiedJulianDay

instance Serialize DiffTime where
  get = fromInteger <$> get
  put = put . diffTimeToPicoseconds

instance Serialize UTCTime where
  get = UTCTime <$> get <*> get
  put (UTCTime day time) = put day >> put time

deriving instance Generic Cookie

instance Serialize Cookie
