> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude hiding (D)
> 
> import Data.VectorSpace
> 
> type D = Diagram Cairo R2
> 
> sq :: D
> sq = (lw 1 . stroke $ arc (pi / 2 :: Rad) (pi :: Rad))
>      `atop` (translate (-0.5, 0.5) . lw 0.5 $ unitSquare)
> 
> phi :: Double
> phi = (1 + sqrt 5) / 2
> 
> step :: D -> D -> D
> step a b = besideAlign (-1,0) (0,1) a b'
>   where b' = rotate (-pi/2 :: Rad) . scale (1/phi) $ b
> 
> besideAlign :: R2 -> R2 -> D -> D -> D
> besideAlign u v a b = beside u b' a
>   where b' = translate (v ^* d) b
>         (Bounds bb) = bounds b
>         (Bounds ba) = bounds a
>         d = (ba v - bb v)
> 
> example :: D
> example = pad 1.05 . freeze . scale 100 $ foldr1 step (replicate 10 sq)
