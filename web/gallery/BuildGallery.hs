{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module BuildGallery where

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

import           System.FilePath             ((</>), (<.>), dropExtension)

import           Control.Arrow               (second)
import           Control.Monad               (mplus)

import qualified Data.Map                    as M

import           System.Console.CmdArgs      hiding (name)

-- If the first argument is 'Just', we're making a thumbnail, so use
-- that as the width and height, and use the 'view' parameters from
-- the LHS file to pick out just a sub-view of the entire diagram.
-- Otherwise, use the width and height specified in the .lhs file and
-- build the entire diagram.
compileExample :: Maybe Double -> String -> String -> IO ()
compileExample mThumb lhs out = do
  f   <- readFile lhs
  let (fields, f') = parseFields f

      w = mThumb `mplus` (read <$> M.lookup "width" fields)
      h = mThumb `mplus` (read <$> M.lookup "height" fields)

      mvs :: Maybe [Double]
      mvs = (map read . splitOn ",") <$> M.lookup "view" fields

      toBuild =
          case (mThumb, mvs) of
            (Just _, Just [vx, vy, vxOff, vyOff]) ->
                "rectEnvelope (p2 " ++ show (vx,vy) ++ ") "
                ++ "(r2 " ++ show (vxOff, vyOff) ++ ") example"
            _ -> "example"

      bopts = mkBuildOpts

#ifdef USE_SVG
                SVG
#else
                Rasterific
#endif

                zero

#ifdef USE_SVG
                (SVGOptions (mkSizeSpec2D w h) Nothing empty)
#else
                -- With raster output, double the resolution so it looks
                -- better on high-res screens
                (RasterificOptions (mkSizeSpec2D ((2*) <$> w) ((2*) <$> h)))
#endif

                & snippets .~ [f']
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
    ParseErr err    -> putStrLn ("Parse error in " ++ lhs) >> putStrLn err
    InterpErr err   -> putStrLn ("Error while compiling " ++ lhs) >>
                       putStrLn (ppInterpError err)
    Skipped _       -> return ()
    OK _ build      ->
#ifdef USE_SVG
      BS.writeFile out (renderBS build)
#else
      JP.savePngImage out (JP.ImageRGBA8 build)
#endif

parseFields :: String -> (M.Map String String, String)
parseFields s = (fieldMap, unlines $ tail rest)
  where (fields, rest) = break (=="---") . tail . lines $ s
        fieldMap       = M.unions
                       . map ((uncurry M.singleton) . second (drop 2) . break (==':'))
                       $ fields

data Build = Build
  { thumb   :: Maybe Double
  , name    :: String
  , outFile :: FilePath
  , inPath  :: FilePath
  }
  deriving (Typeable, Data)

build :: Build
build = Build
  { thumb   = def
  , name    = def &= argPos 0
  , outFile = def &= argPos 1
  , inPath  = def &= argPos 2
  }

main :: IO ()
main = do
  opts <- cmdArgs build
  let name'   = dropExtension (name opts)
      lhsName = inPath opts </> name' <.> "lhs"
  compileExample (thumb opts) lhsName (outFile opts)
