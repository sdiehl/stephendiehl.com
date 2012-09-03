{-# LANGUAGE OverloadedStrings #-}

module Tikz (
    tikzPipeline,
    doTikz
) where

import System.IO
import System.Exit
import System.Process
import System.Posix.Files
import System.FilePath.Posix
import Data.String
import Control.Monad
import Text.Pandoc
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)

tikzClass = ["commute"]
texEngine = "pdflatex"
{-tex_engine = "lualatex"-}
--tex_engine = "xelatex"

tikzCmd     = ["--output-directory=_cache" , "--jobname="]
inkscapeCmd = ["--export-plain-svg" ]
output = "images/"

preamble  = readFile "preamble.tex"
postamble = readFile "postamble.tex"

ljoin = makeRelative "." . join


imageBlock :: FilePath -> Block
imageBlock fname = Plain [Image [] ("/" ++ fname, "")]

svgify :: FilePath -> IO Block
svgify file = do
    (err, outs, _) <- readProcessWithExitCode "inkscape" [file, "--export-plain-svg", output] ""
    case err of
      ExitFailure _ -> do
          putStrLn "Inkscape Failed"
          return $ imageBlock "invalid tikz"
      ExitSuccess -> 
          -- If you want to inline the svg
          {-readFile "output.svg"-}
          return $ imageBlock output
      where
          output = replaceExtension file "svg"

texcompile :: String -> IO String
texcompile tex =  do
    (err, outs, _) <- readProcessWithExitCode texEngine [ "-output-directory", output, "-jobname", jobname] tex
    case err of
      ExitFailure _ -> do
          putStrLn "LaTeX Failed"
          hPutStr stderr outs 
          return ""
      ExitSuccess ->
          return $ ljoin [output, replaceExtension jobname ".pdf"]
      where 
          jobname = show . md5 $ pack tex

mash :: String -> IO String
mash contents = do
    x <- preamble
    y <- postamble
    return $ concat [x, contents, y]

tikzPipeline :: String -> IO Block
tikzPipeline = mash >=> texcompile >=> svgify

doTikz :: Block -> IO Block
doTikz (CodeBlock (id, ["commute"], namevals) contents) = tikzPipeline contents
doTikz x = return x

