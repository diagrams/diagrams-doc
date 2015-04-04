{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module BuildBanner where

#ifdef USE_SVG
import qualified Data.ByteString.Lazy        as BS
import           Data.Text                   (empty)
import           Diagrams.Backend.SVG
import           Lucid.Svg                   (renderBS)
#else
import qualified Codec.Picture               as JP
import           Diagrams.Backend.Rasterific
#endif

import           Diagrams.Prelude            hiding (def)

import           Diagrams.Builder            hiding (Build (..))

import           Data.List.Split

import qualified System.FilePath             as FP

import           Control.Arrow               (second)
import           Control.Monad               (mplus)

import qualified Data.Map                    as M

import           System.Console.CmdArgs      hiding (name)

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

#ifdef USE_SVG
                SVG
#else
                Rasterific
#endif

                zero

#ifdef USE_SVG
                (SVGOptions (mkSizeSpec2D (Just w) (Just h)) [] empty)
#else
                -- With raster output, double the resolution so it looks
                -- better on high-res screens
                (RasterificOptions (mkSizeSpec2D (Just (2*w)) (Just (2*h))))
#endif

                & snippets .~ [f]
                & imports  .~

#ifdef USE_SVG
                  [ "Diagrams.Backend.SVG", "Diagrams.Backend.SVG.CmdLine" ]
#else
                  [ "Diagrams.Backend.Rasterific", "Diagrams.Backend.Rasterific.CmdLine" ]
#endif

                & diaExpr .~ toBuild
                & decideRegen .~ alwaysRegenerate -- XXX use hashedRegenerate?

  res <- buildDiagram bopts

  case res of
    ParseErr err    -> putStrLn ("Parse error in " ++ hs) >> putStrLn err
    InterpErr err   -> putStrLn ("Error while compiling " ++ hs) >>
                       putStrLn (ppInterpError err)
    Skipped _       -> return ()
    OK _ build      ->
#ifdef USE_SVG
      BS.writeFile out (renderBS build)
#else
      JP.savePngImage out (JP.ImageRGBA8 build)
#endif

data Build = Build { name :: String, outFile :: String }
  deriving (Typeable, Data)

build :: Build
build = Build { name = def &= argPos 0, outFile = def &= argPos 1 }

main :: IO ()
main = do
  opts <- cmdArgs build
  let name'   = FP.dropExtension (name opts)
      hsName = (FP.<.>) name' "hs"
  compileExample hsName (outFile opts)
