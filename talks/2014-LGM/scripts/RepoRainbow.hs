{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Data.List.Split
import           Data.Time
import           System.Locale                  (defaultTimeLocale)

type Committer = String
data Commit = Commit Committer UTCTime
  deriving Show
type RepoHist = [Commit]

parseCommit :: (String, String) -> Maybe Commit
parseCommit (name, t) = Commit name <$> parseTime defaultTimeLocale "%s" t

parseHist :: String -> Maybe RepoHist
parseHist = mapM parseCommit . map (\[x,y] -> (x,y)) . chunksOf 2 . lines

drawHist :: RepoHist -> Diagram B R2
drawHist = undefined
  -- XXX bucket commits by week (map from # of weeks prior to today?)
  -- draw rectangles with height corresponding to # of commits in a given week

  -- later, break up rectangles by color based on committer?  Need a
  -- scheme for assigning committer names to colors.

main :: IO ()
main = do
  lib <- readFile "logs/diagrams-lib.txt"
  case parseHist lib of
    Nothing -> putStrLn "No parse"
    Just hist -> defaultMain (drawHist hist)
