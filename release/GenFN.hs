{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances
           , MultiParamTypeClasses
  #-}

import Text.Pandoc
import Text.Pandoc.Generic

import Data.List (intersperse)

import Control.Monad.Writer
import Supply

import Data.Generics

everywhere'M :: Monad m => GenericM m -> GenericM m
everywhere'M f x = do x' <- f x
                      gmapM (everywhere'M f) x'

topDownM :: (Monad m, Data a, Data b) => (a -> m a) -> b -> m b
topDownM f = everywhere'M (mkM f)

type SW = SupplyT Int (Writer [(Int, Target)])

addFN :: Pandoc -> Pandoc
addFN = appendLinks . runWriter . flip evalSupplyT [1..] . topDownM genFNs

appendLinks :: (Pandoc, [(Int, Target)]) -> Pandoc
appendLinks (Pandoc meta bs, links) = Pandoc meta (bs ++ [linkPara])
  where linkPara = Para (intersperse LineBreak ls)
        ls       = map genLink links
        genLink (n, (url,_)) = Str $ "[" ++ show n ++ "] " ++ url

genFNs :: [Inline] -> SW [Inline]
genFNs = fmap concat . mapM genFN

genFN :: Inline -> SW [Inline]
genFN (Link is targ) = do
  n <- supply
  lift $ tell [(n,targ)]
  return (is ++ [Space, Str $ "[" ++ show n ++ "]"])
genFN i = return [i]

------------------------------------------------------------
-- General format reading/writing
------------------------------------------------------------

readMD :: String -> Pandoc
readMD = readMarkdown defaultParserState

writeText :: Pandoc -> String
writeText = writePlain defaultWriterOptions

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = interact (writeText . addFN . readMD)
