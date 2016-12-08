{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2014
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

import Hakyll
import Text.Pandoc
import Data.Monoid
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Contexts
-------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  `mappend` mathCtx
  `mappend` defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

archiveCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Archives"
  `mappend` defaultContext

indexCtx =
  constField "title" "Home"
  `mappend` defaultContext

-------------------------------------------------------------------------------
-- Rules
-------------------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

static :: Rules ()
static = do
  match "fonts/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "images/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "js/*" $ do
    route idRoute
    compile $ copyFileCompiler

index :: Rules ()
index = do
  match "index.md" $ do
    route   $ setExtension "html"
    compile $ compiler
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html"    postCtx
      >>= relativizeUrls

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= loadAndApplyTemplate "templates/default.html"    postCtx
      >>= relativizeUrls

archive :: Rules ()
archive = do
  create ["posts.html"] $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/list.html" (archiveCtx posts)
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

feeds :: Rules ()
feeds = do
  let getPosts = recentFirst =<< loadAllSnapshots "posts/*" "content"
  let config = FeedConfiguration
        { feedAuthorEmail = "stephen.m.diehl@gmail.com"
        , feedAuthorName = "Stephen Diehl"
        , feedDescription = "Stephen Diehl's blog."
        , feedRoot = "http://www.stephendiehl.com"
        , feedTitle = "Stephen Diehl"
        }
  let context = mconcat
        [ bodyField "description"
        , postCtx
        ]
  create ["feed.atom"] $ do
    route idRoute
    compile $ do
      posts <- getPosts
      renderAtom config context posts
  create ["feed.rss"] $ do
    route idRoute
    compile $ do
      posts <- getPosts
      renderRss config context posts

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  posts
  feeds
  archive
  index
  templates
