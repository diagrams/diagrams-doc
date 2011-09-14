{-# LANGUAGE StandaloneDeriving
  , DeriveDataTypeable
  #-}

module Build where

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Language.Haskell.Interpreter

import System.IO
import System.FilePath
import System.Directory

import Data.Typeable

deriving instance Typeable Any

setDiagramImports :: MonadInterpreter m => String -> m ()
setDiagramImports m = do
    loadModules [m]
    setTopLevelModules [takeBaseName m]
    setImports [ "Prelude"
               , "Diagrams.Prelude"
               , "Graphics.Rendering.Diagrams.Core"
               , "Diagrams.Backend.Cairo"
               , "Data.Monoid"
               ]

compileExample :: String -> Options Cairo R2 -> IO ()
compileExample m opts = do
    x <- runInterpreter $ do
      setDiagramImports m
      d <- interpret "pad 1.1 example" (as :: Diagram Cairo R2)
      liftIO . fst $ renderDia Cairo opts d
    case x of
      Left e -> ppError e
      Right _ -> return ()

ppError :: InterpreterError -> IO ()
ppError (UnknownError e) = putStrLn $ "UnknownError: " ++ e
ppError (WontCompile es) = putStr . unlines . map errMsg $ es
ppError (NotAllowed e)   = putStrLn $ "NotAllowed: " ++ e
ppError (GhcException e) = putStrLn $ "GhcException: " ++ e  -- TODO: can we actually recover from this?

-- | Given the diagram's source code and options for the cairo
--   backend, build the diagram (in the context of standard imports) and
--   render it as requested
buildDiagram :: String -> Options Cairo R2 -> IO ()
buildDiagram source opts = do
  tmpDir <- getTemporaryDirectory
  (tmp, h) <- openTempFile tmpDir "Diagram.lhs"
  hPutStr h (diagramFileHeader $ takeBaseName tmp)
  hPutStr h source
  hClose h
  compileExample tmp opts
  removeFile tmp

diagramFileHeader :: String -> String
diagramFileHeader modName = unlines $
  [ "> {-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}"
  , "> module " ++ modName ++ " where"
  , "> import Diagrams.Prelude"
  , "> import Data.Typeable"
  ]
