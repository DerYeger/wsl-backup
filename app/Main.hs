module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [dir, d] -> backupSingleDistribution dir d
    [dir] -> backupAllDistributions dir
    _ -> interactiveBackup
