module Main where

import qualified App
import           System.Directory
import           Types

import           Control.Exception    (finally)
import           Control.Monad.Reader
import qualified Data.ByteString      as B
import           Data.Serialize
import           System.Environment
import           System.Exit
import           System.IO

configFile :: FilePath
configFile = "nexusdownloader.cfg"

main :: IO ()
main = do
  env <- getStoredEnv
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr "No bundle file given!"
      exitFailure
    (bundle:_) -> (flip runReaderT env $ App.main bundle)
                    `finally` (B.writeFile configFile $ encode env)


getStoredEnv :: IO Env
getStoredEnv = do
  exists <- doesFileExist configFile
  if exists then do
    file <- B.readFile configFile
    case decode file of
      Right env -> pure env
      _         -> mkDefaultEnv
  else
    mkDefaultEnv
