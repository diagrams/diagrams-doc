{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module BuildBanner where

import qualified Codec.Picture               as JP
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (def)
import           Diagrams.Builder            hiding (Build (..))
import           Data.List.Split             (splitOn)
import           System.FilePath             ((</>), (<.>), dropExtension)
import           Control.Arrow               (second)
import           Control.Monad               (mplus)
import qualified Data.Map                    as M
import           System.Console.CmdArgs      hiding (name)
import           System.IO                   (hPutStrLn, stderr)

compileExample :: String -> String -> IO ()
compileExample hs out = do
  f   <- readFile hs
  let w = 900 :: Double
      h = 350 :: Double

      toBuild = unwords ["rectEnvelope"
                        ,     "(p2 ", show (-w/2,-h/2), ")"
                        ,     "(r2", show (w,h), ")"
                        ,     "(centerXY $ sized (mkSizeSpec2D (Just ", show w, ") Nothing) diagram)"
                        ]
      bopts = mkBuildOpts

                Rasterific
                zero
                -- With raster output, double the resolution so it looks
                -- better on high-res screens
                (RasterificOptions (mkSizeSpec2D (Just (2*w)) (Just (2*h))))
                & snippets .~ [f]
                & imports  .~
                  [ "Diagrams.Backend.Rasterific", "Diagrams.Backend.Rasterific.CmdLine" ]
                & diaExpr .~ toBuild
                & decideRegen .~ alwaysRegenerate -- XXX use hashedRegenerate?

  res <- buildDiagram bopts

  case res of
    ParseErr err    -> do
      hPutStrLn stderr ("Parse error in " ++ hs)
      hPutStrLn stderr err
    InterpErr err   -> do
      hPutStrLn stderr ("Error while compiling " ++ hs)
      hPutStrLn stderr (ppInterpError err)
    Skipped _       -> return ()
    OK _ build      ->
      JP.savePngImage out (JP.ImageRGBA8 build)

data Build = Build
  { name :: String
  , outFile :: FilePath
  , inPath :: FilePath
  } deriving (Typeable, Data)

build :: Build
build = Build
  { name    = def &= argPos 0
  , outFile = def &= argPos 1
  , inPath  = def &= argPos 2
  }

main :: IO ()
main = do
  opts <- cmdArgs build
  let name'  = dropExtension (name opts)
      hsName = inPath opts </> name' <.> "hs"
  compileExample hsName (outFile opts)
