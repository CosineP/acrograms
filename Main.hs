module Main where

import           Anagrams
import qualified Data.ByteString.Char8 as BS
import           System.Environment (getArgs)

main :: IO ()
main = do
  dict <- readDict
  [input] <- getArgs
  mapM_ (putStrLn . BS.unpack) $ anagrams dict (BS.pack input) (BS.pack "wfaoie")
