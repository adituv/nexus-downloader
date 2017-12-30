module Extra
  ( module Reexport
  , showBS
  -- * IO Utilities
  , withLabel
  , getUsername
  , getPassword
  ) where

import           Data.ByteString       as Reexport (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Semigroup        as Reexport
import           Data.Text             as Reexport (Text)
import           GHC.Generics          as Reexport (Generic)
import           System.IO             (hFlush, stdout)
import           System.IO.Echo        (withoutInputEcho)

showBS :: Show a => a -> ByteString
showBS x = pack $ show x

withLabel :: String -> IO a -> IO a
withLabel lab action = do
  putStr $ lab ++ ": "
  hFlush stdout
  action

getUsername :: IO String
getUsername = withLabel "Username" getLine

getPassword :: IO String
getPassword = withLabel "Password" (withoutInputEcho getLine)
