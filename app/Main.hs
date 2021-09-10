module Main where

import Lib
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [dir, d] -> backupSingleDistribution dir d
    [dir] -> backupAllDistributions dir
    _ -> do
      putStrLn "Invalid arguments. Expected target directory or target directory and distribution name"
      exitFailure
