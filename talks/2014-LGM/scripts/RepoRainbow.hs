{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Data.List.Split
import qualified Data.Map                       as M
import           Data.Time
import           System.Directory               (getDirectoryContents)
import           System.FilePath                ((</>))
import           System.Locale                  (defaultTimeLocale)

type Committer = String
-- Int is # of weeks ago
data Commit = Commit { committer  :: Committer
                     , commitTime :: UTCTime
                     , weeksAgo   :: Integer
                     }
  deriving Show
type RepoHist = [Commit]

parseCommit :: Day -> (String, String) -> Maybe Commit
parseCommit today (name, t) = do
  pt <- parseTime defaultTimeLocale "%s" t
  let ptDay = case utcToLocalTime utc pt of
                (LocalTime d _) -> d
  return $ Commit name pt (diffDays today ptDay `div` 7)

parseHist :: Day -> String -> Maybe RepoHist
parseHist today = mapM (parseCommit today) . map (\[x,y] -> (x,y)) . chunksOf 2 . lines

drawHist :: RepoHist -> Diagram B R2
drawHist hist = map (drawWeek . flip M.lookup buckets) [0 .. numWeeks] # reverse # hcat
  where
    drawWeek :: Maybe RepoHist -> Diagram B R2
    drawWeek Nothing   = rect 1 0.2 # lw 0 # fc blue
    drawWeek (Just wk) = roundedRect 1 (fromIntegral $ length wk) 0.4 # lw 0 # fc blue
    numWeeks :: Integer
    numWeeks = maximum (M.keys buckets)
    buckets :: M.Map Integer RepoHist
    buckets = foldr (\c -> M.insertWith (++) (weeksAgo c) [c]) M.empty hist

  -- later, break up rectangles by color based on committer?  Need a
  -- scheme for assigning committer names to colors.

main :: IO ()
main = do
  files <- getDirectoryContents "logs"
  now <- getCurrentTime
  let (LocalTime today _) = utcToLocalTime utc now
  dias <- mapM (genDia today) files
  defaultMain (vcat dias)

genDia :: Day -> FilePath -> IO (Diagram B R2)
genDia today file = do
  log <- readFile ("logs" </> file)
  case parseHist today log of
    Nothing   -> putStrLn ("No parse: " ++ file) >> return mempty
    Just hist -> return (drawHist hist)
