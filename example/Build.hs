-- TODO:
--  * PNG + other format generation
--  * Extract width and height from MD headers

import System.FilePath
import System.Directory
import System.Environment
import System.Cmd
import Control.Monad
import Data.List
import qualified Data.Map as M

data BeInfo = BeInfo { beMod :: String, beExt :: String } 
            deriving (Eq, Ord, Show)
                         
backends :: M.Map String BeInfo
backends = M.fromList 
           [("cairo-pdf", 
             BeInfo "Diagrams.Backend.Cairo.CmdLine" "pdf"),
            ("cairo-png", 
             BeInfo "Diagrams.Backend.Cairo.CmdLine" "png")]

backendModule :: String -> String
backendModule backend = beMod $ backends M.! backend

backendExtension :: String -> String
backendExtension backend = beExt $ backends M.! backend

main :: IO ()
main = do
  -- Command line argument: backend description
  cmd <- getArgs
  if (length cmd /= 1 || M.notMember (head cmd) backends)
    then error "Must supply single backend descriptor"
    else do
      -- Glob *.lhs to get list of examples
      lhss <- (getDirectoryContents ".") >>= 
              (\fs -> return $ filter ((== ".lhs") . takeExtension) fs)
      
      -- Glob *.hs to get list of examples (remove Build!)
      hss <- (getDirectoryContents ".") >>= 
             (\fs -> return $ filter (/= "Build.hs") fs) >>=
             (\fs -> return $ filter ((== ".hs") . takeExtension) fs)
  
      -- Make output directory
      let backend = head cmd
      createDirectoryIfMissing False backend
  
      -- Process .lhs files into output directory, then build and run
      -- examples.
      forM_ hss (\f -> copyFile f (backend </> f))
      forM_ lhss (convertLhs backend)
      setCurrentDirectory backend
      forM_ lhss $ 
        \f -> do
          rawSystem "ghc" ["--make", f]
          putStrLn $ "Running " ++ (dropExtension f)
          rawSystem ("." </> dropExtension f) 
            ["-o", replaceExtension f (backendExtension backend)]
  
convertLhs :: FilePath -> FilePath -> IO ()
convertLhs backend lhs = do
  ls <- readFile lhs
  writeFile (backend </> lhs) $ (unlines . process . lines) ls
  where process = (++ mainProgram) . fixImports . removeModule
        removeModule = filter (\l -> not ("> module" `isPrefixOf` l))
        mainProgram = [">", "> main = defaultMain example"]
        fixImports = addAfter "> import Diagrams.Prelude" $ 
                     "> import " ++ backendModule backend

addAfter :: String -> String -> [String] -> [String]
addAfter _ _ [] = []
addAfter b a (x:xs)
  | b `isPrefixOf` x = x:a:(addAfter b a xs)
  | otherwise = x:(addAfter b a xs)
