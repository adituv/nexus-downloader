{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module App where

import           Prelude                hiding (fail)

import           Extra
import           Types

import           Nexus.API              as Nexus

import           Control.Monad.Fail
import           Control.Monad.IfElse
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), runReaderT)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.IO           as T
import           Lens.Micro.Platform
import           System.Exit
import           System.IO

main :: (MonadReader Env m, MonadIO m, MonadFail m) => FilePath -> m ()
main bundlefile = do
  nenv <- view nexusEnv
  bundle <- liftIO $ eitherDecode' <$> BL.readFile bundlefile
  flip runReaderT nenv . runNexus $ do
    unlessM Nexus.validate $ do
      user <- liftIO $ getUsername
      pass <- liftIO $ getPassword
      unlessM (Nexus.login user pass) $ do
        -- TODO: retries?
        liftIO $ hPutStrLn stderr "Login failed.  Aborting."
        liftIO $ exitFailure
  case bundle of
    Left msg -> fail msg
    Right b -> do
      liftIO . T.putStrLn $ "All files for: " <> b ^. name
      forOf_ (files . each) b $ \f -> do
        liftIO $ T.putStrLn ("* " <> f ^. filename)
        info <- flip runReaderT nenv . runNexus $ Nexus.getDownloadInfo (f ^. fileid)
        case info of
          []     -> fail "No downloads available"
          (dl:_) -> liftIO $ putStrLn ("  - " <> Nexus._URI dl)
