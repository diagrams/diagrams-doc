{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Control.Monad                  (filterM)
import           Data.List                      (sortBy)
import           Data.List.Split
import qualified Data.Map                       as M
import           Data.Ord                       (comparing)
import           Data.Time
import           System.Directory               (doesFileExist,
                                                 getDirectoryContents)
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

parseCommit :: Day -> (String, String) -> Either String Commit
parseCommit today (name, t) =
  case mpt of
    Nothing -> Left t
    Just pt ->
      let ptDay = case utcToLocalTime utc pt of
                    (LocalTime d _) -> d
      in  Right $ Commit name pt (diffDays today ptDay `div` 7)
  where
    mpt = parseTime defaultTimeLocale "%s" t

parseHist :: Day -> String -> Either String RepoHist
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
  files  <- getDirectoryContents "logs"
  files' <- filterM doesFileExist (map ("logs" </>) files)
  now <- getCurrentTime
  let (LocalTime today _) = utcToLocalTime utc now
  repos <- mapM (getRepoHist today) files'
  let reposSorted = reverse $ sortBy (comparing (maximum . map weeksAgo)) repos
  let reposDia = vcat' (with & catMethod .~ Distrib & sep .~ 30) . map (alignR . drawHist) $ reposSorted
      allDia   = alignR . drawHist . concat $ repos
  defaultMain $ (reposDia)

getRepoHist :: Day -> FilePath -> IO RepoHist
getRepoHist today file = do
  log <- readFile file
  case parseHist today log of
    Left s     -> putStrLn ("No parse: " ++ file ++ s) >> return mempty
    Right hist -> return hist
