> import Diagrams.Prelude hiding (D)
> 
> import Data.List
> 
> type D = Diagram Cairo R2
> 
> colors = [blue, green, red, yellow, purple]
> 
> type Disk  = Int
> type Stack = [Disk]
> type Hanoi = [Stack]
> type Move  = (Int,Int)
> 
> renderDisk :: Disk -> D
> renderDisk n = rect (fromIntegral n + 2) 1
>                # lc black
>                # fc (colors !! n)
>                # lw 0.1
> 
> renderStack :: Stack -> D
> renderStack s = disks `atop` post
>   where disks = (vcat . map renderDisk $ s)
>                 # alignB
>         post  = rect 0.8 6
>                 # lw 0
>                 # fc saddlebrown
>                 # alignB
> 
> renderHanoi :: Hanoi -> D
> renderHanoi = hcat' with {catMethod = Distrib, sep = 7} . map renderStack
> 
> solveHanoi :: Int -> [Move]
> solveHanoi n = solveHanoi' n 0 1 2
>   where solveHanoi' 0 _ _ _ = []
>         solveHanoi' n a b c = solveHanoi' (n-1) a c b ++ [(a,c)]
>                               ++ solveHanoi' (n-1) b a c
> 
> doMove :: Move -> Hanoi -> Hanoi
> doMove (x,y) h = h''
>   where (d,h')         = removeDisk x h
>         h''            = addDisk y d h'
>         removeDisk x h = (head (h!!x), modList x tail h)
>         addDisk y d    = modList y (d:)
> 
> modList i f l  = let (xs,(y:ys)) = splitAt i l in xs ++ (f y : ys)
> 
> hanoiSequence :: Int -> [Hanoi]
> hanoiSequence n = scanl (flip ($)) [[0..n-1], [], []] (map doMove (solveHanoi n))
> 
> renderHanoiSeq :: [Hanoi] -> D
> renderHanoiSeq = vcat' with { sep = 2 } . map renderHanoi
> 
> example = pad 1.1 $ renderHanoiSeq (hanoiSequence 4) # centerXY
