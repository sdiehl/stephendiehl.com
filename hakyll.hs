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

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  posts
  archive
  index
  templates

{-main :: IO ()-}
{-main = hakyll $ do-}
    {-match "images/*" $ do-}
        {-route   idRoute-}
        {-compile copyFileCompiler-}

    {-match "css/*" $ do-}
        {-route   idRoute-}
        {-compile compressCssCompiler-}

    {-match "files/*" $ do-}
        {-route   idRoute-}
        {-compile copyFileCompiler-}

    {-match "templates/*" $ compile templateCompiler-}

    {-match "index.md" $ do-}
        {-route   $ setExtension "html"-}
        {-compile $ pageCompiler-}
            {->>> applyTemplateCompiler "templates/default.html"-}
            {->>> relativizeUrlsCompiler-}

    {-match "posts/*" $ do-}
        {-route   $ setExtension "html"-}
        {-compile $ py_pre-}
            {->>> arr readPage-}
            {->>> addDefaultFields-}
            {->>> pageRenderPandocWith defaultHakyllParserState pandocOptions-}
            {->>> applyTemplateCompiler "templates/default.html"-}
            {->>> relativizeUrlsCompiler-}

    {-match "pages/*" $ do-}
        {-route   $ setExtension "html"-}
        {-compile $ pageCompiler-}
            {->>> applyTemplateCompiler "templates/default.html"-}
            {->>> relativizeUrlsCompiler-}

    {-match "posts.html" $ route idRoute-}
    {-create "posts.html" $ constA mempty-}
        {->>> arr (setField "title" "All posts")-}
        {->>> requireAllA "posts/*" addPostList-}
        {->>> applyTemplateCompiler "templates/list.html"-}
        {->>> applyTemplateCompiler "templates/default.html"-}
        {->>> relativizeUrlsCompiler-}

{-addPostList :: Compiler (Page String, [Page String]) (Page String)-}
{-addPostList = setFieldA "posts" $-}
    {-arr chronological-}
        {->>> require "templates/post.html" (\p t -> map (applyTemplate t) p)-}
        {->>> arr mconcat-}
        {->>> arr pageBody-}
