module Main where

import Lib
import System.Environment (getArgs)
import System.Process (readCreateProcess, shell)

main :: IO ()
main = do
  args <- getArgs
  readCreateProcess (shell "chcp.com 65001") ""
  case args of 
    [dir, d] -> backupSingleDistribution dir d
    [dir] -> backupAllDistributions dir
    _ -> interactiveBackup
