{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Diagrams.Builder

import qualified System.FilePath as FP
import System.Environment

import Data.List (isPrefixOf)
import Control.Arrow ((&&&), second)

import qualified Data.Map as M
import Data.List.Split
import Data.Maybe

compileExample :: String -> String -> IO ()
compileExample lhs outFile = do
  let fmt = case FP.takeExtension outFile of
              ".png" -> PNG
              ".svg" -> SVG
              ".ps"  -> PS
              ".pdf" -> PDF
              _     -> PNG

  f   <- readFile lhs
  let (fields, f') = parseFields f

      w = read <$> M.lookup "width" fields
      h = read <$> M.lookup "height" fields

  res <- buildDiagram
           Cairo
           zeroV
           (CairoOptions outFile (mkSizeSpec w h) fmt)
           f'
           "example"
           []
           [ "Diagrams.Backend.Cairo" ]
  case res of
    Left err              -> putStrLn ("Parse error in " ++ lhs) >> putStrLn err
    Right (Left err)      -> putStrLn ("Error while compiling " ++ lhs) >>
                             putStrLn (ppInterpError err)
    Right (Right (act,_)) -> act

parseFields :: String -> (M.Map String String, String)
parseFields s = (fieldMap, unlines $ tail rest)
  where (fields, rest) = break (=="---") . tail . lines $ s
        fieldMap       = M.unions
                       . map ((uncurry M.singleton) . second (drop 2) . break (==':'))
                       $ fields

main = do
  [name, outFile] <- getArgs
  let name'   = FP.dropExtension name
      lhsName = (FP.<.>) name' "lhs"
  compileExample lhsName outFile