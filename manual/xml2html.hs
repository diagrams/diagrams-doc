import Text.Docutils.CmdLine

import Control.Arrow

import Text.Docutils.Util

import Text.Docutils.Writers.HTML
import Text.Docutils.Transformers.Haskell

main :: IO ()
main = docutilsCmdLine diagramsManual

diagramsManual = 
  doTransforms [ linkifyHackage
               , linkifyModules "diagrams-lib"
               , highlightHS
               ] 
  >>> xml2html 
  >>> doTransforms [ styleFile "default.css"
                   , styleFile "syntax.css"
                   ]