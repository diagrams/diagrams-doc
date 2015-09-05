{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

colors = brewerSet YlOrRd 9

boxes :: Tree () -> Diagram B
boxes t = undefined

main :: IO ()
main = do
  Just t <- runGenM 30 0.15 genTree
  mainWith (boxes t # frame 1)
