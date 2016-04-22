module BuildGallery where

import qualified Codec.Picture               as JP
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude
import           Diagrams.Builder
import           Data.List.Split             (splitOn)
import           Control.Arrow               (second)
import           Control.Monad               (mplus)
import qualified Data.Map                    as M
import           System.IO                   (hPutStrLn, stderr)

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
                Rasterific
                zero
                -- With raster output, double the resolution so it looks
                -- better on high-res screens
                (RasterificOptions (mkSizeSpec2D ((2*) <$> w) ((2*) <$> h)))
                & snippets .~ [f']
                & imports  .~
                  [ "Diagrams.Backend.Rasterific", "Diagrams.Backend.Rasterific.CmdLine" ]
                & diaExpr .~ toBuild
                & decideRegen .~ alwaysRegenerate -- XXX use hashedRegenerate?

  res <- buildDiagram bopts

  case res of
    ParseErr err    -> do
      hPutStrLn stderr ("Parse error in " ++ lhs)
      hPutStrLn stderr err
    InterpErr err   -> do
      hPutStrLn stderr ("Error while compiling " ++ lhs)
      hPutStrLn stderr (ppInterpError err)
    Skipped _       -> return ()
    OK _ build      ->
      JP.savePngImage out (JP.ImageRGBA8 build)

parseFields :: String -> (M.Map String String, String)
parseFields s = (fieldMap, unlines $ tail rest)
  where (fields, rest) = break (=="---") . tail . lines $ s
        fieldMap       = M.unions
                       . map ((uncurry M.singleton) . second (drop 2) . break (==':'))
                       $ fields
