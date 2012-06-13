import System.Process


tikz_cmd = [
    "pdflatex"
    , "--output-directory=_cache"
    , "--jobname="
]

svg_cmd = [
     "inkscape"
   , "--export-plain-svg"
]

renderTikzScript :: -> Pandoc.Block -> IO Pandoc.Block
renderTikzScript blk@(Pandoc.CodeBlock ("tikz", classes, _) rawScript) = do
  putStrLn "Rendering equation graph (tikz)"

  let digestInput = rawScript

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
  {- Render Latex -}
  case s1 of
    ExitFailure _ ->
            putStrLn "Could not render Tikz picture:"
            return blk
    ExitSuccess -> do
            -- Convert the temporary file to a PNG.
            (s2, out2, err) <- readProcessWithExitCode "inkscape", svg_cma ++ imagePath

             {- Render SVG -}
            case s2 of
              ExitFailure _ -> do
                      putStrLn "Could not render Tikz picture:"
                      return blk
              ExitSuccess ->
                  return $ Pandoc.Para [Pandoc.RawInline "html" $
                                              concat [ "<img src=\"/images/"
                                                     , imageFilename
                                                     , "\" class=\""
                                                     , intercalate " " classes
                                                     , "\">"
                                                     ]
                                       ]
