module Tikz
    ( tikzProcessor
    )
where

import Data.List
    ( intercalate
    )
import Data.Digest.Pure.SHA
    ( showDigest
    , sha1
    )
import Data.ByteString.Lazy.Char8
    ( pack
    )
import System.Process
    ( readProcessWithExitCode
    )
import System.Exit
    ( ExitCode(..)
    )
import qualified Text.Pandoc as Pandoc
{-import MB.Types-}
{-import qualified MB.Files as Files-}

tikzProcessor :: Processor
tikzProcessor =
    nullProcessor { preProcessPost = Just renderTikz
                  }

renderTikzScript :: Blog
                 -> Pandoc.Block
                 -> IO Pandoc.Block
renderTikzScript config blk@(Pandoc.CodeBlock ("tikz", classes, _) rawScript) = do
  putStrLn "Rendering equation graph (tikz)"

  let digestInput = rawScript

      -- Generate an image name in the images/ directory of the blog
      -- data directory.  Use a hash of the preamble name and script
      -- contents so we can avoid rendering the image again if it
      -- already exists.
      hash = showDigest $ sha1 $ pack digestInput
      imageFilename = "tikz-" ++ hash ++ ".png"
      imagePath = Files.imageFilename config imageFilename
      preamble = unlines [ "\\documentclass{article}"
                         , "\\usepackage{tikz}"
                         , "\\usepackage{pgfplots}"
                         , "\\pgfrealjobname{tmp}"

                         , "\\begin{document}"
                         , "\\begin{figure}"
                         , "\\beginpgfgraphicnamed{testfigure}"
                         , "\\begin{tikzpicture}"
                         ]
      postamble = unlines [ "\\end{tikzpicture}"
                          , "\\endpgfgraphicnamed"
                          , "\\end{figure}"
                          , "\\end{document}"
                          ]

      latexSource = preamble ++ rawScript ++ postamble

  writeFile "/tmp/tmp.tex" latexSource

  (s1, out1, _) <- readProcessWithExitCode "pdflatex" [ "-output-directory", "/tmp"
                                                      , "--jobname", "testfigure", "/tmp/tmp.tex"] ""
  case s1 of
    ExitFailure _ -> putStrLn out1 >> return blk
    ExitSuccess -> do
            -- Convert the temporary file to a PNG.
            (s2, out2, err) <- readProcessWithExitCode "convert" [ "-density", "120"
                                                                 , "-quality", "100"
                                                                 , "/tmp/testfigure.pdf"
                                                                 , imagePath
                                                                 ] ""

            case s2 of
              ExitFailure _ -> do
                      putStrLn "Could not render Tikz picture:"
                      putStrLn "Equation was:"
                      putStrLn latexSource
                      putStrLn "dvipng output:"
                      putStrLn out2
                      putStrLn err
                      return blk
              ExitSuccess ->
                  return $ Pandoc.Para [Pandoc.RawInline "html" $
                                              concat [ "<img src=\"/generated-images/"
                                                     , imageFilename
                                                     , "\" class=\""
                                                     , intercalate " " classes
                                                     , "\">"
                                                     ]
                                       ]
renderTikzScript _ b = return b
