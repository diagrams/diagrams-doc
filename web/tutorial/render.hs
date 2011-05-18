{-# LANGUAGE ViewPatterns #-}

-- See http://johnmacfarlane.net/pandoc/scripting.html

import Text.Pandoc

import Data.Char

renderAll :: Pandoc -> IO Pandoc
renderAll = processWithM render
  where render :: Block -> IO Block
        render (CodeBlock _ (lines -> d:ds)) | "[dia]" `isPrefixOf` d
          = do let d'   = dropWhile isSpace (drop 5 d)
                   prog = mkProg ["DiagramsTutorial"] ds d'
               writeFile "dia_tmp.hs" prog
               status <- bracket (openFile "dia_tmp.out" WriteMode) hClose $ \h ->
                 waitForProcess =<< runProcess "runhaskell"
                   ["dia_tmp.hs", "-o", "dia_tmp.png", "-w", "400"]
                   Nothing Nothing Nothing Nothing (Just h)

               when (status == ExitSuccess) $ do
                 system "cp dia_tmp.hs" -- XXX


               return $ Para [Image [] (url, title)]

        render b = return b

mkProg :: [String] -> [String] -> String -> String
mkProg imports defs dia = unlines $
  [ "import Diagrams.Prelude"
  , "import Diagrams.Backend.Cairo.CmdLine"
  ]
  ++ map ("import " ++) imports
  ++ defs
  ++
  [ "main = defaultMain (pad 1.1 $ " ++ dia ++ ")" ]

------------------------------------------------------------
-- General format reading/writing
------------------------------------------------------------

readLHS :: String -> Pandoc
readLHS = readMarkdown defaultParserState { stateLiterateHaskell = True }

writeLHS :: Pandoc -> String
writeLHS = writeMarkdown defaultWriterOptions { writerLiterateHaskell = True }

writeHTML :: Pandoc -> String
writeHTML = writeHtmlString defaultWriterOptions { writerLiterateHaskell = True }

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  src <- readLHS <$> getContents
  src' <- renderAll src
  putStr . writeLHS $ src'
