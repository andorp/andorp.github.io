module Main where

import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Blog.Entries
import Blog.HTML
import Blog.Model (FileProperties(..))

main :: IO ()
main = do
  (inputDir, htmlOutputDir) <- checkAndGetParams
  blog <- browse inputDir
  render htmlOutputDir (dropPrefixOfFP inputDir <$> blog)

checkAndGetParams :: IO (FilePath, FilePath)
checkAndGetParams = do
  args <- getArgs
  when (length args /= 2) $ usage
  mapM_ checkDirectory args
  return (args !! 0, args !! 1)

-- Print usage
usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ concat ["Usage ", name, " markdown-dir html-output-dir"]
  exitFailure

-- Check if directory exist
checkDirectory :: FilePath -> IO ()
checkDirectory dir = do
  exits <- doesDirectoryExist dir
  unless exits $ do
    putStrLn $ "Directory does not exist: " ++ dir
    exitFailure

-- Helpers

dropPrefix :: FilePath -> FilePath -> FilePath
dropPrefix prefix path
  | isPrefixOf prefix path = drop (length prefix) path
  | otherwise = error "Expected to have prefix"

dropPrefixOfFP :: FilePath -> FileProperties -> FileProperties
dropPrefixOfFP prefix (FileProperties path utcTime) = FileProperties (dropPrefix prefix path) utcTime
