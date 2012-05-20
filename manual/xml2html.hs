
import System.FilePath ( (<.>), (</>) )
import System.IO

import Data.VectorSpace ( zeroV )
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Builder ( buildDiagram, BuildResult(..), ppInterpError, hashedRegenerate )
import Diagrams.TwoD.Size ( SizeSpec2D(Dims) )
import Text.Docutils.CmdLine
import Text.Docutils.Transformers.Haskell
import Text.Docutils.Util
import Text.Docutils.Writers.HTML
import Text.XML.HXT.Core hiding (when)

main :: IO ()
main = do
  (modMap, nameMap) <- buildPackageMaps
                       [ "diagrams-core"
                       , "active"
                       , "diagrams-lib"
                       , "diagrams-cairo"
                       , "diagrams-contrib"
                       , "vector-space"
                       , "vector-space-points"
                       ]
  docutilsCmdLine (diagramsManual modMap nameMap)

diagramsManual modMap nameMap =
  doTransforms [ linkifyHackage
               , linkifyModules modMap
               , highlightInlineHS
               , highlightBlockHS
               , compileDiagrams
               , compileDiagramsLHS
               , linkifyHS nameMap modMap
               ]
  >>> xml2html
  >>> doTransforms [ styleFile "css/default.css"
                   , styleFile "css/syntax.css"
                   ]

compileDiagrams :: XmlT (IOSLA (XIOState ()))
compileDiagrams = onElemA "literal_block" [("classes", "dia")] $
  eelem "div"
    += attr "class" (txt "exampleimg")
    += compileDiaArr

-- | Compile code blocks intended to generate both a diagram and the
--   syntax highlighted code.
compileDiagramsLHS :: XmlT (IOSLA (XIOState ()))
compileDiagramsLHS = onElemA "literal_block" [("classes", "dia-lhs")] $
  eelem "div"
    += attr "class" (txt "dia-lhs")
    += (compileDiaArr <+> highlightBlockHSArr)

compileDiaArr :: (ArrowXml (~>), ArrowIO (~>)) => XmlT (~>)
compileDiaArr =
  getChildren >>>
  getText >>>
  arrIO compileDiagram >>>
  eelem "div"
    += attr "style" (txt "text-align: center")
    += (eelem "img"
         += attr "src" mkText
       )

-- | Compile the literate source code of a diagram to a .png file with
--   a file name given by a hash of the source code contents
compileDiagram :: String -> IO String
compileDiagram src = do
  res <- buildDiagram
           Cairo
           zeroV
           (CairoOptions "default.png" (Dims 500 200) PNG)
           [src]
           "pad 1.1 example"
           ["DeriveDataTypeable"]
           [ "Diagrams.TwoD.Types"      -- WHY IS THIS NECESSARY =(
           , "Graphics.Rendering.Diagrams.Points"
               -- GHC 7.2 bug?  need  V (Point R2) = R2  (see #65)
           , "Diagrams.Backend.Cairo"
           , "Diagrams.Backend.Cairo.Internal"
           , "Data.Typeable"
           ]
           (hashedRegenerate
             (\hash opts -> opts { cairoFileName = mkFile hash })
             "images"
           )
  case res of
    ParseErr err    -> putStrLn ("\nError while parsing\n" ++ src) >>
                       putStrLn err >>
                       return "default.png"
    InterpErr ierr  -> putStrLn ("\nError while interpreting\n" ++ src) >>
                       putStrLn (ppInterpError ierr) >>
                       return "default.png"
    Skipped hash    -> putStr "." >> hFlush stdout >> return (mkFile hash)
    OK hash (act,_) -> putStr "O" >> hFlush stdout >> act >> return (mkFile hash)
 where
  mkFile base = "images" </> base <.> "png"