{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr, (>>^))
import Control.Applicative
import Data.Monoid (mempty, mconcat)

import Hakyll
import Tikz

import Text.Pandoc
import System.IO.Unsafe

renderPandocThrough ::(Compiler (Page Pandoc) (Page Pandoc)) -> Compiler Resource (Page String)
renderPandocThrough p = readPageCompiler >>> addDefaultFields >>> arr applySelf 
                        >>> pageReadPandoc >>> p >>> arr (fmap writePandoc)

catTransformer = cached "Commutative Diagrams" $ renderPandocThrough $ catInplace

catInplace :: Compiler (Page Pandoc) (Page Pandoc)
catInplace =  timedCompiler "Commutative diagrams" $ unsafeCompiler $ \(Page md b) -> do
    compiled <- bottomUpM doTikz $ b
    return $ (Page md compiled)

-- Python tipy preprocessor
py_pre :: Compiler Resource String
py_pre = getResourceString >>> unixFilter "tipy" ["--preprocess"]

-- MathJax as Math backend
pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "index.md" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ py_pre
            >>> arr readPage
            >>> addDefaultFields
            >>> pageRenderPandocWith defaultHakyllParserState pandocOptions
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "pages/*" $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/list.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr chronological
        >>> require "templates/post.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
