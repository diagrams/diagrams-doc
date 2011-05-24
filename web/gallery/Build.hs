{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Language.Haskell.Interpreter

import System.Environment

import Data.Typeable

import Data.List (isPrefixOf)
import Control.Arrow ((&&&))

deriving instance Typeable Any

diagramWitness :: Diagram Cairo R2
diagramWitness = error "Diagram Witness"

setDiagramImports :: MonadInterpreter m => String -> m ()
setDiagramImports m = do
    loadModules [m]
    setTopLevelModules [m]
    setImports [ "Prelude"
               , "Diagrams.Prelude"
               , "Graphics.Rendering.Diagrams.Core"
               , "Diagrams.Backend.Cairo"
               , "Data.Monoid"
               ]

compileExample :: String -> String -> Int -> Int -> IO ()
compileExample m outFile w h = do
    x <- runInterpreter $ do
      setDiagramImports m
      d <- interpret "example" diagramWitness
      liftIO . fst $ renderDia Cairo (CairoOptions outFile (PNG (w,h))) d
    case x of
      Left e -> ppError e
      Right _ -> return ()


ppError :: InterpreterError -> IO ()
ppError (UnknownError e) = putStrLn $ "UnknownError: " ++ e
ppError (WontCompile es) = putStr . unlines . map errMsg $ es
ppError (NotAllowed e)   = putStrLn $ "NotAllowed: " ++ e
ppError (GhcException e) = putStrLn $ "GhcException: " ++ e  -- TODO: can we actually recover from this?

getDimens fileName = extractDimens `fmap` readFile fileName

extractDimens = (getW &&& getH) . lines
  where
    getW = getField "width"
    getH = getField "height"
    getField f = read . drop (length f + 2) . head . filter ((f ++ ":") `isPrefixOf`)

main = do
  [name, outFile] <- getArgs
  (w,h) <- getDimens (name ++ ".lhs")
  compileExample name outFile w h