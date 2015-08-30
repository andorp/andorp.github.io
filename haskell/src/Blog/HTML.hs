{-# LANGUAGE OverloadedStrings #-}
module Blog.HTML (
    render
  ) where

import Prelude hiding (div, head)

import Control.Applicative ((<$>), (<*>))
import Data.Function (on)
import Data.List (sortBy)
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory as Dir
import System.FilePath as FilePath
import Text.Blaze.Html5 hiding (menu)
import Text.Blaze.Html5.Attributes as A hiding (id, title)
import Text.Pandoc.Definition (Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Options (def)
import Text.Pandoc.Writers.HTML
import Text.Blaze.Html.Renderer.String

import Blog.Model hiding (entry)


render :: FilePath -> BlogT FileProperties -> IO ()
render outDir blog = do
  let frame = createPageFrame blog
  let topicsDiv = topics blog
  let content = renderHtml $ frame NavInPlace $ do
                  pandoc2panel $ b_summary blog
                  topicsDiv
  writeFile (outDir </> "index.html") content
  renderPages outDir frame blog


renderPages :: FilePath -> (NavPath -> Html -> Html) -> BlogT FileProperties -> IO ()
renderPages outDir frame = blogCatamorphism render where
  topicName fp pandoc = do
    Dir.createDirectoryIfMissing True $ outDir </> markdownPathToHTMLDir fp
    return (\content -> writeFile (outDir </> (markdownPathToHTMLPathFP fp)) (renderHtml $ frame NavInPlace content)
           , pandoc
           )

  entry fp pandoc = do
    writeFile (outDir </> (markdownPathToHTMLPathFP fp))
              (renderHtml . frame NavBackward $ pandoc2html pandoc)
    return (fp, firstHeader pandoc)
  
  topicNameEntryList _fp topicName entryList = do
    (topicPage, pandoc) <- topicName
    headers <- entryList
    topicPage $ do
      pandoc2panel pandoc
      topicsList headers

  entryList = (return [], \x xs -> (:) <$> x <*> xs)
  
  topic = (topicName, entry, entryList, topicNameEntryList)
  combineTopicList fp _summary ls = ls
  render = (topic, sequence_, combineTopicList)

  sequence_ = (return (), (>>))


data NavPath
  = NavForward String
  | NavInPlace
  | NavBackward
  deriving (Show, Eq)


data NavPathAlgebra m = NavPathAlgebra {
    np_forward  :: String -> m
  , np_inplace  :: m
  , np_backward :: m
  }


navPathCatamorphism :: NavPathAlgebra m -> NavPath -> m
navPathCatamorphism algebra n =
  case n of
    NavForward m -> np_forward  algebra m
    NavInPlace   -> np_inplace  algebra
    NavBackward  -> np_backward algebra


navPrefix = navPathCatamorphism (NavPathAlgebra {np_forward = id, np_inplace = "", np_backward = ".."})


menu :: BlogT FileProperties -> NavPath -> Html
menu blog navPath = blogCatamorphism blogMenu blog
  where
    blogMenu = (topicToListItem, topicMenu, wrapMenu)
    topicToListItem = (topicNameToListItem, entryToVoid, listToVoid, ignoreEntryList)
    topicNameToListItem fp text = li $ filePropertiesToLink fp $ fromString $ firstHeader text
    entryToVoid = (\_ _ -> ())
    listToVoid = ((), \_ _ -> ())
    topicMenu = (return (), (>>))
    ignoreEntryList _hole topicName _entryList = topicName
    wrapMenu _hole _summary menuItems = ul ! class_ "nav nav-pills nav-stacked" $ menuItems

    filePropertiesToLink fp html = a ! href (markdownPathToHTMLPath $ navPrefix navPath </> path fp) $ html


topicsList :: [(FileProperties, String)] -> Html
topicsList entries = div ! class_ "list-group" $
  mapM_ link . reverse $ sortBy (compare `on` modificationTime . fst) entries
  where
    link (fp, headLine) = a ! href (markdownPathToHTMLPath $ path fp)
                            ! class_ "list-group-item" $ fromString headLine


topics :: BlogT FileProperties -> Html
topics blog =
  topicsList . Prelude.map (\e -> (e_hole e, firstHeader $ e_lines e))
             . concatMap t_entries
             $ b_topics blog


createPageFrame :: BlogT FileProperties -> NavPath -> Html -> Html
createPageFrame blog navPath content = do
  htmlHeader
  body $ container $ do
    topNavbar navPath
    row $ do
      colMd3 $ menu blog navPath
      colMd9 $ content

pandoc2panel :: Pandoc -> Html
pandoc2panel pandoc = do
  div ! class_ "panel panel-default" $ do
    div ! class_ "panel-heading" $ firstHeader pandoc
    div ! class_ "panel-body" $ pandoc2html $ removeFirstHeader pandoc

-- Bootstrap helpers

htmlHeader :: Html
htmlHeader = do
  head $ do
    title "Blog"
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    link ! rel "stylesheet" ! href "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    script ! src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js" $ return ()
    script ! src "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" $ return ()


topNavbar :: NavPath -> Html
topNavbar navPath = do
  div ! class_ "navbar navbar-default" $ do
    div ! class_ "container-fluid" $ do
      div ! class_ "navbar-header" $ do
        a ! class_ "navbar-brand" ! href (fromString $ navPrefix navPath </> "index.html") $ "andorp"


container :: Html -> Html
container body = div ! class_ "container" $ body

row :: Html -> Html
row c = div ! class_ "row" $ c

colMd3 :: Html -> Html
colMd3 c = div ! class_ "col-md-3" $ c

colMd9 :: Html -> Html
colMd9 c = div ! class_ "col-md-9" $ c

markdownPathToHTMLDir :: FileProperties -> String
markdownPathToHTMLDir = FilePath.dropExtension . path

markdownPathToHTMLPath :: (IsString string) => FilePath -> string
markdownPathToHTMLPath = fromString . flip FilePath.addExtension "html" . FilePath.dropExtension

markdownPathToHTMLPathFP :: FileProperties -> FilePath
markdownPathToHTMLPathFP = markdownPathToHTMLPath . path

-- HTML helpers

pandoc2html :: Pandoc -> Html
pandoc2html = writeHtml def

text2html :: Text -> Html
text2html = fromString . Text.unpack

-- Pandoc helpers

isHeader (Header _ _ _) = True
isHeader _              = False

firstHeader :: (IsString string) => Pandoc -> string
firstHeader (Pandoc _meta blocks) =
  case filter isHeader blocks of
    []                     -> error "No header is found"
    (Header _ _ inlines:_) -> fromString $ inlinesToString inlines

removeFirstHeader :: Pandoc -> Pandoc
removeFirstHeader (Pandoc meta blocks) = Pandoc meta (removeFirst isHeader blocks)
  where
    removeFirst _p []     = []
    removeFirst p  (x:xs)
      | p x       = xs
      | otherwise = x : removeFirst p xs

inlinesToString :: [Inline] -> String
inlinesToString = concatMap strInlineToString


strInlineToString :: Inline -> String
strInlineToString (Str str) = str
strInlineToString Space     = " "
strInlineToString x         = error $ "No string inline: " ++ show x
