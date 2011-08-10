import Text.Docutils.CmdLine

import Control.Arrow

import Text.Docutils.Util

import Text.Docutils.Writers.HTML
import Text.Docutils.Transformers.Haskell

main :: IO ()
main = do
  modMap <- buildModuleMap [ "diagrams-core"
                           , "diagrams-lib"
                           , "diagrams-cairo"
                           ]
  docutilsCmdLine (diagramsManual modMap)

diagramsManual modMap =
  doTransforms [ linkifyHackage
               , linkifyModules modMap
               , highlightHS
               ]
  >>> xml2html
  >>> doTransforms [ styleFile "default.css"
                   , styleFile "syntax.css"
                   ]