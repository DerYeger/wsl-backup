{-# LANGUAGE OverloadedStrings #-}

module Lib (backupSingleDistribution, backupAllDistributions, interactiveBackup) where

import Control.Exception (try)
import Data.List (break, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Text (pack, unpack, replace)
import System.Directory (listDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension)
import System.Process (callCommand, readCreateProcess, shell)

type Dir = String
type Distribution = String

checkTargetDir :: String -> IO ()
checkTargetDir dir = do
  result <- try (listDirectory dir) :: IO (Either IOError [FilePath])
  case result of
    Left err -> do
      putStrLn $ "❌ Directory '" ++ dir ++ "' does not exist"
      exitFailure
    Right files -> putStrLn $ "💽 Target directory: " ++ dir

checkDistribution:: Distribution -> IO ()
checkDistribution d = do
  exists <- distributionExists d
  if exists
    then
      putStrLn $ "🔎 Distribution: " ++ d
    else do
      putStrLn $ "❌ Distribution '" ++ d ++ "' does not exist"
      exitFailure

wslExport :: String -> String
wslExport = ("wsl --export " ++)

makeArgs :: Dir -> Distribution -> String
makeArgs dir d = d ++ " " ++ dir ++ "\\" ++ d ++ ".tar"

isTar :: FilePath -> Bool
isTar = isSuffixOf ".tar"

exportDistribution :: Dir -> Distribution -> IO ()
exportDistribution dir d = do
  putStrLn $ "\n⏳ Exporting '" ++ d ++ "' to '" ++ dir ++ "'. This may take a while."
  callCommand . wslExport $ makeArgs dir d
  putStrLn $ "✅ '" ++ d ++ "' exported successfully"

removeSpecialChars :: String -> String
removeSpecialChars = filter (\c -> c /= '\NUL' && c /= '\n')
  
removeDefault :: String -> String
removeDefault = unpack . replace " (Default)" "" . pack

sanitizeOutput :: String -> String
sanitizeOutput = removeDefault . removeSpecialChars

extractDistributions :: String -> [Distribution]
extractDistributions s = filter (not . null) . tail $ splitOn "\r" s

printDistributions :: [Distribution] -> IO ()
printDistributions ds = do
  putStrLn $ "\n🔎 " ++ show (length ds) ++ " distributions found:"
  mapM_ putStrLn ds

distributions :: IO [Distribution]
distributions = fmap (extractDistributions . sanitizeOutput) (readCreateProcess (shell "wsl -l --all") "")

distributionExists :: Distribution -> IO Bool
distributionExists d = fmap (elem d) distributions

backupSingleDistribution :: Dir -> Distribution -> IO ()
backupSingleDistribution dir d = do
  checkDistribution d
  checkTargetDir dir
  exportDistribution dir d

backupAllDistributions :: Dir -> IO ()
backupAllDistributions dir = do
  putStrLn "🔧 No distribution specified. Creating backups for all distributions."
  checkTargetDir dir
  ds <-  distributions
  printDistributions ds
  mapM_ (exportDistribution dir) ds

interactiveBackup :: IO ()
interactiveBackup = putStrLn "💽 Enter target directory: " >> getLine >>= (\dir -> putStr "\n" >> backupAllDistributions dir)
