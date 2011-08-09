import Text.Docutils.CmdLine

import Control.Arrow

import Text.Docutils.Util

import Text.Docutils.Writers.HTML
import Text.Docutils.Transformers.Haskell

main :: IO ()
main = docutilsCmdLine (doTransforms [hackage] >>> xml2html)