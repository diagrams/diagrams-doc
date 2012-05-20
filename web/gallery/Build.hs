{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal -- due to GHC export bug in 7.4

import Diagrams.Builder

import qualified System.FilePath as FP

import Control.Arrow (second)
import Control.Monad (mplus)

import qualified Data.Map as M

import System.Console.CmdArgs hiding (name)

-- If the first argument is 'Just', use that as the width and height.
-- Otherwise, use the width and height specified in the .lhs file.
compileExample :: Maybe Double -> String -> String -> IO ()
compileExample mThumb lhs out = do
  let fmt = case FP.takeExtension out of
              ".png" -> PNG
              ".svg" -> SVG
              ".ps"  -> PS
              ".pdf" -> PDF
              _     -> PNG

  f   <- readFile lhs
  let (fields, f') = parseFields f

      w = mThumb `mplus` (read <$> M.lookup "width" fields)
      h = mThumb `mplus` (read <$> M.lookup "height" fields)

  res <- buildDiagram
           Cairo
           zeroV
           (CairoOptions out (mkSizeSpec w h) fmt)
           [f']
           "example"
           []
           [ "Diagrams.Backend.Cairo" ]
           alwaysRegenerate  -- XXX use hashedRegenerate?
  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ lhs) >> putStrLn err
    InterpErr err   -> putStrLn ("Error while compiling " ++ lhs) >>
                       putStrLn (ppInterpError err)
    Skipped _       -> return ()
    OK _ (act,_)    -> act

parseFields :: String -> (M.Map String String, String)
parseFields s = (fieldMap, unlines $ tail rest)
  where (fields, rest) = break (=="---") . tail . lines $ s
        fieldMap       = M.unions
                       . map ((uncurry M.singleton) . second (drop 2) . break (==':'))
                       $ fields

data Build = Build { thumb :: Maybe Double, name :: String, outFile :: String }
  deriving (Typeable, Data)

build :: Build
build = Build { thumb = def, name = def &= argPos 0, outFile = def &= argPos 1 }

main :: IO ()
main = do
  opts <- cmdArgs build
  let name'   = FP.dropExtension (name opts)
      lhsName = (FP.<.>) name' "lhs"
  compileExample (thumb opts) lhsName (outFile opts)