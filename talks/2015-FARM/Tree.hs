{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Tree where

import           Control.Monad
import           Control.Monad.Random

import           Data.Colour.Palette.BrewerSet
import qualified Data.Foldable                 as F
import           Data.List
import           Data.Ord

import           Diagrams.LinearMap            (amap)
import           Diagrams.Prelude              hiding (size)
import           Diagrams.ThreeD.Projection
import           Diagrams.ThreeD.Transform     (translateZ)

import           Codec.Picture                 (GifDelay)

import           Linear.Matrix                 ((!*!))

import           BoltzmannTrees

import qualified Debug.Trace                   as T

full 0 = Leaf ()
full n = Branch () (full (n-1)) (full (n-1))

levels t = takeWhile (/= 0) $ go t
  where
    go (Leaf _) = 1 : repeat 0
    go (Branch _ l r) = 1 : zipWith (+) (go l) (go r)

depth (Leaf _) = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

depthByFactor t p = go ls (floor (fromIntegral s * p)) 0
  where
    ls = levels t
    s = size t

    go []     t n = n
    go (a:as) t n
        | a < t     = go as (t-a) (n+1)
        | otherwise = n

stickZ p = origin ~~ (0 ^& 0 ^& 1)
        <> p # translate unitZ

branch = 20
tilt   = 10

vee l r = stickZ l # rotX (  branch  @@ deg)
       <> stickZ r # rotX ((-branch) @@ deg)

rotZ a = transform (aboutZ a)
rotX a = transform (aboutX a)

onPoint n d = withName n $ \(location -> p) -> atop (d `place` p)

tree3D (Leaf _)       = mempty
tree3D (Branch _ l r) = vee vl vr
  where
    vl = tree3D l # rotZ (90 @@ deg) # rotX (  tilt  @@ deg) # scale 0.9
    vr = tree3D r # rotZ (90 @@ deg) # rotX ((-tilt) @@ deg) # scale 0.9

veeWibble l r = do
    lf <- (+0.8) . (*0.4) <$> getRandom
    rf <- (+0.8) . (*0.4) <$> getRandom
    return $ stickZ l # scale lf # rotX (  branch  @@ deg)
          <> stickZ r # scale rf # rotX ((-branch) @@ deg)

tree3DWibble (Leaf _)       = return mempty
tree3DWibble (Branch _ l r) = do
    vl <- scale 0.9 . rotX (  tilt  @@ deg) . rotZ (90 @@ deg) <$> tree3DWibble l
    vr <- scale 0.9 . rotX ((-tilt) @@ deg) . rotZ (90 @@ deg) <$> tree3DWibble r

    veeWibble vl vr
{-
veeWibble' f = do
     lf <- (+0.8) . (*0.4) <$> getRandom
     rf <- (+0.8) . (*0.4) <$> getRandom
     let u = unitZ # scale lf # rotX (  branch  @@ deg)
         v = unitZ # scale rf # rotX ((-branch) @@ deg)
     return ( f (origin ~~ u) <> f (origin ~~ v)
            , undefined -- f (origin .+^ u)
            , undefined -- f (origin .+^ v)
            )

tree3DWibble' f (Leaf _)       = return $ Leaf (f mempty)
tree3DWibble' f (Branch _ l r) = do
    (w,ol,or) <- veeWibble' f
    Branch w
           <$> tree3DWibble' (f . translate ol . scale 0.9 . rotX (  tilt  @@ deg) . rotZ (90 @@ deg)) l
           <*> tree3DWibble' (f . translate or . scale 0.9 . rotX ((-tilt) @@ deg) . rotZ (90 @@ deg)) r
-}

veeWibble' o f = do
    lf <- (+0.8) . (*0.4) <$> getRandom
    rf <- (+0.8) . (*0.4) <$> getRandom
    let vl = (origin ~~ (origin .+^ unitZ)) # scale lf # rotX (  branch  @@ deg) # f
        vr = (origin ~~ (origin .+^ unitZ)) # scale rf # rotX ((-branch) @@ deg) # f
    return ( trailLike (vl `at` o)
          <> trailLike (vr `at` o)
           , o .+^ trailOffset vl
           , o .+^ trailOffset vr
           , f . rotX (  branch  @@ deg) . scale lf
           , f . rotX ((-branch) @@ deg) . scale rf
           )

tree3DWibble' :: Tree a -> IO (Tree (Path V3 Double))
tree3DWibble' t = go origin id t
  where
    go _ _ (Leaf _)       = return $ Leaf mempty
    go o f (Branch _ l r) = do
      (w,lo,ro,fl,fr) <- veeWibble' o f
      Branch w <$> go lo (fl . scale 0.9 . rotX (  tilt  @@ deg) . rotZ (90 @@ deg)) l
               <*> go ro (fr . scale 0.9 . rotX ((-tilt) @@ deg) . rotZ (90 @@ deg)) r

viewVector = -(V3 8.4 6 3.2)
m  = lookAt (-viewVector) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

withPerspective' :: _ => Path V3 Double -> Diagram b
withPerspective' d = stroke $ deform pd (translateZ (-1) d)

color n = let cs = brewerSet RdYlGn 9 in cs !! (n `mod` 9)

traceShowId a = T.trace (show a) a

treeColors t = go t 0
  where
    m = depth t
    m' = depthByFactor t 0.3
    cs = brewerSet RdYlGn 9

    color n = cs !! (floor (fromIntegral (n * 8) / fromIntegral m') `min` 8)

    go (Leaf _)       _ = []
    go (Branch _ l r) n = (color n : go l (n+1)) ++ (color n : go r (n+1))

withPerspective :: _ => [Kolor] -> Path V3 Double -> Diagram b
withPerspective cs d = mconcat . zipWith (\c -> lc c . strokeLocT) cs . concat
                     . explodePath $ deform pd (translateZ (-1) d)

getCenter = boxCenter . boundingBox

tree2D t r = tree3D t # rotZ r # withPerspective (treeColors t)

tree2D' t r = tree3D t # fmap (rotZ r) # fmap withPerspective'

colorByDepth t = go t 0
  where
    m = depth t
    m' = depthByFactor t 0.3
    cs = brewerSet RdYlGn 9

    color n = cs !! (floor (fromIntegral (n * 8) / fromIntegral m') `min` 8)

    go (Leaf x)        d = Leaf (color d, x)
    go (Branch x l r ) d = Branch (color d, x) (go l (d+1)) (go r (d+1))


spinAndProject t = \r -> fmap ((\x -> (getCenter x, withPerspective' x)) . rotZ r) t

to2D = fmap (mconcat . sortZ . F.toList . colorByDepth) . spinAndProject
    where
        sortZ = map applyColor . sortBy s

        s = comparing (fmap (dot v . (.-. p)) . fst . snd)
        p = origin .+^ (-viewVector)
        v = signorm viewVector

        applyColor (k,(_,d)) = d # lc k

tree2DWibble t = do
    w <- tree3DWibble t
    return $ \r -> w # rotZ r # withPerspective (treeColors t)

frameCount = 100
delay = 6 :: GifDelay

spin f = map (,delay) . allRotations $ frame
  where
    !bb = boundingBox . mconcat . allRotations $ f
    allRotations f = [f (n @@ turn) | i <- [0..frameCount]
                                    , let n = fromIntegral i / fromIntegral frameCount]
    frame r = f r # withEnvelope bb # bgFrame 0.05 skyblue

sky bb = mkLinearGradient (mkStops [(darkgreen,0,1), (white,0.1,1), (skyblue,1,1)])
                          a b GradPad
    where
      (a,b) = maybe (0 ^& 0, 1 ^& 1) id (getCorners bb)

mainRandWibbleSeed = do
    let seed = 31
        Just t = seedGenM seed 1000 0.15 genTree
    w <- to2D <$> tree3DWibble' t
--    return (spin w)
    return (w (0 @@ turn) # bgFrame 0.05 skyblue)
