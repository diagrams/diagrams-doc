{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow        ((+++))
import           Control.Lens
import           Data.Functor
import           Github.Repos.Commits

main :: IO ()
main = do
  s <- readFile "diagrams-lib-commits"
  let coms = read s :: [Commit]
  coms' <- mapM fillInCommit coms
  writeFile "diagrams-lib-commits-full" (show coms')

fillInCommit :: Commit -> IO (Either Commit Commit)
fillInCommit c@(Commit { commitSha = sha })
  = (const c +++ deletePatches) <$> commit' (Just (GithubBasicAuth "byorgey" "XXX")) "diagrams" "diagrams-lib" sha

cFiles g c@(Commit { commitFiles = fs }) = (\fs' -> c { commitFiles = fs' }) <$> g fs
fPatch g f@(File { filePatch = p }) = (\p' -> f { filePatch = p' }) <$> g p

deletePatches :: Commit -> Commit
deletePatches = cFiles . mapped . fPatch .~ ""
