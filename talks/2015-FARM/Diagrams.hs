{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Diagrams where

import Diagrams.Prelude
import Diagrams.Backend.PGF

import Data.Char
import Data.List hiding (sort)
import Data.Maybe

import           BinLayout
import           BoltzmannTrees
import qualified Data.Tree                 as T
import qualified Diagrams.TwoD.Layout.Tree as LT
import           Dyck
import           TreeBox
import           Triangulation
import Tree

-- import Diagrams.TwoD.Sunburst
-- , sunburst (treeToTree t)    -- doesn't seem to work right

n = 20

treeToTree :: Tree a -> T.Tree a
treeToTree (Leaf a) = T.Node a []
treeToTree (Branch a l r) = T.Node a (map treeToTree [l,r])

Just theTree = seedGenM 22 (2*n + 1) 0 genTree

treeVisuals :: _ => [[Bool]] -> IO (Diagram b)
treeVisuals hl = do
  let radTree
        = LT.renderTree (const (circle 0.1 # fc black)) (~~)
        . LT.radialLayout
        . treeToTree
        $ theTree
  wibbleTree <- mainRandWibbleSeed
  return
    $ vsep 2
    . map (centerX . hsep 2 . map (sized (dims2D 10 10) . centerXY))
--    . zipWith (zipWith (\b d -> d # frame 2 # if b then bg lightcyan else id)) hl
    $
    [ [ treeBoxes theTree, drawTree theTree, drawDyck False theTree ]
    , [ triangulation theTree, radTree, wibbleTree ]
    ]

-- Some parts from http://www.iis.sinica.edu.tw/~scm/pub/bwtJFP.pdf
bwt ws = (bwn, bwp)
  where
    bwp = map last . lexsort . rots $ ws
    bwn = succ . fromJust . findIndex (== ws) . lexsort . rots $ ws

rots xs = take (length xs) (iterate lrot xs)

lrot xs = tail xs ++ [head xs]

sortby f = sortBy (\x y -> if x `f` y then LT else GT)

lexsort ls = sortby (leq (length ls)) ls
  where
    leq 0  _      _     = True
    leq k (x:xs) (y:ys) = x < y || (x == y && leq (k-1) xs ys)

recreate :: Ord a => Int -> [a] -> [[a]]
recreate 0 ys = map (const []) ys
recreate k ys = sortby leq (join ys (recreate (k-1) ys))
  where leq us vs = head us <= head vs
        join xs xss = [y:ys | (y,ys) <- zip xs xss]

unbwt :: Ord a => Int -> [a] -> [a]
unbwt t ys = take (length ys) (thread (spl ys t))
  where thread (x,j) = x:thread (spl ys j)
        spl ys t = fromJust $ lookup t (zip [1..] (sortby (<=) (zip ys [1..])))

-----------
alphabet' :: Double -> Int -> Diagram B
alphabet' w (-1) = square 1 # lc red # lwG w # withEnvelope (circle 1 :: D V2 Double)
alphabet' w i    = c # lc (acolor i) # lwG w
  where
    m  = abs i `mod` 10
    c  = mconcat [circle (fromIntegral (m + 1 - r) / fromIntegral (m + 1)) | r <- [0..m]]

alphabet = alphabet' 0.05

acolor :: Int -> Colour Double
acolor (-1) = red
acolor i = cs !! (abs i `mod` l)
  where
    l  = length cs
    cs = [red, orange, yellow, green, blue, purple]

bwtDia :: Diagram B
bwtDia = squared
  where
    vsep = 0.1
    hsep = 0.1

    vcatSep s = vcat' (with & sep .~ s)
    hcatSep s = hcat' (with & sep .~ s)
    hcatSepCenter s = hcatSep s . map centerXY
    vhcatSep sv sh = vcatSep sv . map (hcatSep sh)

    squared = vcatSep (1.5)
               [ alignL top
               , alignL (hcatSep hsep (reverse . map (alphabet' 0.1) $ s))
               , alignL bottom # translate ((-2-hsep) ^& 0)
               ]
      where
        top    =         [ inputToBWT,          bwtToRLE ] # hcatSepCenter 2 # reflectY
        bottom = reverse [ bwtToInput, reflectX bwtToRLE ] # hcatSepCenter 2 # reflectY
                                                           # rotate (1/2 @@ turn)

    inputToBWT =
      [ block rs # reflectX    -- Rotations of s
      , sorting 7 head rs rs'
      , block rs'              -- Sorted rotations
                               -- of s
      ]
      # hcatSepCenter hsep

    buildUnbwt =
      [ block [[a,b] | (a,b) <- ps] # reflectX  -- spl table
      , sorting 23.1 fst ps ps'
      , block [[a,b,i] | (i,(a,b)) <- ips]      -- continued
      , mconcat [ (0 ^& 0) ~~ ((7-(2+2*hsep)+fromIntegral j * (2+hsep)) ^& 0)
                    # lc (acolor x)
                    # withEnvelope (strutY 2 :: D V2 Double)
                    # lwG 0.05
                    # moveTo (0 ^& (fromIntegral (length p - i) * (2+vsep)))
                | (j,(i,x,_)) <- zip [1..] ts
                ]
      ]
      # hcatSepCenter hsep

    bwtToInput = [ buildUnbwt, threads n p ] # map alignB # hcatSep 7

    bwtToRLE = block . groupBy (==) $ p

    threads n p = vcatSep (-1)
                    [ alignL (hcatSep hsep (map (alphabet . snd) is)) -- map snd is ~ s
                    , alignL (reflectY $ hcatSep (-2) $ take (length p * 2 - 1) ds)
                    ]
      where
        (is,ds) = mconcat [ ([(i,x)],
                              [ moveTo (0 ^& (fromIntegral i * (2+vsep))) (centerXY (block [[x,j]]))
                              , connectW 2 i j # lc (acolor j)
                              ])
                          | (i,x,j) <- ts
                          ]

    row = hcatSep hsep
    block = vhcatSep hsep vsep . map (map alphabet)

    sorting w f rs rs' = reflectY $ mconcat
            [ connectH w i j # lc (acolor (f r))
            | (i,r) <- zip [0 :: Int ..] rs
            , let j = fromJust . findIndex (== r) $ rs'
            ]
    connectH w i j = bez (0 ^& f i) (w*2/5 ^& f i) (w*3/5 ^& f j) (w ^& f j) # lwG 0.05
      where
        f x = fromIntegral x * (2+vsep)

    connectW w i j
      | abs (i - j) < 2 = strutX w
      | otherwise
        = bez (0 ^& (f i + y)) (0 ^& (f i + d*2/5)) (w ^& (f j - d*3/5)) (w ^& (f j - y)) # lwG 0.05
      where
        d = f j - f i
        y = signum d
        f x = fromIntegral x * (2+vsep)

    rs  = rots s
    rs' = lexsort rs

    s = (-1) : map ((subtract (ord '0')) . ord) "101103107109113"
    (n,p) = bwt s
    ps  = zip p [1..]
    ps' = sortby (<=) ps
    ips = zip [1..] ps'
    spl t = fromJust $ lookup t ips
    thread i (x,j) = (i,x,j) : thread j (spl j)
    ts = take (length p) (thread n (spl n))

bez a b c d = trailLike $ (fromSegments [bezier3 (b .-. a) (c .-. a) (d .-. a)]) `at` a
