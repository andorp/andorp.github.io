module Blog.Entries (
    browse
  ) where

-- Each directory represents a topic which has a special file
-- namely topic.md this is a small description of the given
-- topic and the title. 

import Control.Applicative
import Control.Monad
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown

import Blog.Model

getFileProperties :: FilePath -> IO FileProperties
getFileProperties path = FileProperties path <$> getModificationTime path

topicFile = "topic.md"
summaryFile = "summary.md"
markdownExtension = ".md"

browse :: FilePath -> IO (BlogT FileProperties)
browse rootDir = do
  fp       <- getFileProperties rootDir
  summary  <- readMarkdownFile (rootDir </> summaryFile)
  contents <- getDirectoryContentsNoDots rootDir
  dirs     <- (filterM doesDirectoryExist contents)
  Blog fp summary <$> mapM browseTopicDir dirs

browseTopicDir :: FilePath -> IO (TopicT FileProperties)
browseTopicDir topicDir = do
  fp <- getFileProperties topicDir
  topicName <- readTopicName topicDir
  entries <- readEntries topicDir
  return $! Topic fp topicName entries

readTopicName :: FilePath -> IO (TopicNameT FileProperties)
readTopicName topicDir = do
  fp <- getFileProperties topicDir
  pandoc <- readMarkdownFile (topicDir </> topicFile)
  return $! TopicName fp pandoc

readMarkdownFile :: FilePath -> IO Pandoc
readMarkdownFile file = do
  content <- readFile file
  return $! either (error . show) id $ readMarkdown def content

readEntry :: FilePath -> IO (EntryT FileProperties)
readEntry file = do
  fp <- getFileProperties file
  Entry fp <$> readMarkdownFile file

listEntryFiles :: FilePath -> IO [FilePath]
listEntryFiles dir = filter (isEntry . snd . splitFileName) <$> getDirectoryContentsNoDots dir

isEntry :: FilePath -> Bool
isEntry filename
  | filename == topicFile = False
  | otherwise = takeExtension filename == markdownExtension

readEntries :: FilePath -> IO [EntryT FileProperties]
readEntries = listEntryFiles >=> (mapM readEntry)

getDirectoryContentsNoDots dir =
  (map (dir </>) . filter notDots) <$> getDirectoryContents dir
  where
    notDots = not . flip elem [".", ".."]
