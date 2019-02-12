module Main where

import           Anagrams
import           System.Environment (getArgs)

main :: IO ()
main = do
  dict <- readDict
  [input] <- getArgs
  mapM_ putStrLn $ anagrams dict input "owifea"

