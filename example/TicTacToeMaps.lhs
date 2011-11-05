Maps of optimal tic-tac-toe play, inspired by similar maps created by
Randall Munroe, http://xkcd.com/832/

> {-# LANGUAGE NoMonomorphismRestriction #-}
> 
> import Diagrams.Prelude hiding (Result,D)
> import Diagrams.Backend.Cairo.CmdLine
> 
> import Data.List.Split (chunk)                   -- cabal install split
> import Data.Maybe (fromMaybe, catMaybes)
> import qualified Data.Map as M
> import Data.Tree
> import Control.Arrow (second, (&&&), (***))
> import Data.Array (assocs)
> 
> import TicTacToeSolve
> 
> type D = Diagram Cairo R2
> 
> x, o :: D
> x = (P (-1,1) ~~ P (1,-1) <> P (1,1) ~~ P (-1,-1))
>   # lw 0.05
>   # lineCap LineCapRound
>   # scale 0.4
>   # freeze
>   # centerXY
> o = circle 0.4
>   # lw 0.05
>   # freeze

Render a list of lists of diagrams in a grid.

> grid :: Double -> [[D]] -> D
> grid s = centerXY
>        . vcat' with {catMethod = Distrib, sep = s}
>        . map (hcat' with {catMethod = Distrib, sep = s})

Given a mapping from (r,c) locations (in a 3x3 grid) to diagrams,
render them in a grid, surrounded by a square.

> renderGrid :: M.Map Loc D -> D
> renderGrid g
>   = (grid 1
>   . chunk 3
>   . map (fromMaybe (phantom x) . flip M.lookup g)
>   $ [ (r,c) | r <- [0..2], c <- [0..2] ])
> 
>     `atop`
>     square 3 # lw 0.02 # freeze

Given a solved game tree, where the first move is being made by the
player for whom the tree is solved, render a map of optimal play.

> renderSolvedP :: Tree (Game, Result) -> D
> renderSolvedP (Node (Game _ p _, _) [])   -- cats game, this player does not
>     = renderPlayer (next p) # scale 3     -- get another move; instead of
>                                           -- recursively rendering this game
>                                           -- just render an X or an O
> renderSolvedP (Node (Game board player1 _, _)
>                     [g'@(Node (Game _ _ (Move _ loc : _), res) conts)])
>     = renderResult res <>    -- Draw a line through a win
>       renderGrid cur   <>    -- Draw the optimal move + current moves
>       renderOtherP g'        -- Recursively render responses to other moves
> 
>   where cur = M.singleton loc (renderPlayer player1 # lc red)  -- the optimal move
>               <> curMoves board                                -- current moves
> 
> renderSolvedP _ = error "renderSolvedP should be called on solved trees only"

Given a solved game tree, where the first move is being made by the
opponent of the player for whom the tree is solved, render a map of
optimal play.

> renderOtherP :: Tree (Game, Result) -> D
> renderOtherP (Node _ conts)
>     -- just recursively render each game arising from an opponent's move in a grid.
>   = renderGrid . M.fromList . map (getMove &&& (scale (1/3) . renderSolvedP)) $ conts
>   where getMove (Node (Game _ _ (Move _ m : _), _) _) = m

Extract the current moves from a board.

> curMoves :: Board -> M.Map Loc D
> curMoves = M.fromList . (map . second) renderPlayer . catMaybes . map strength . assocs

Render a line through a win.

> renderResult :: Result -> D
> renderResult (Win _ 0 ls) = winLine # freeze
>   where winLine :: D
>         winLine = fromVertices (map (P . conv) ls)
>                   # lw 0.2
>                   # lc blue
>                   # lineCap LineCapRound
>         conv (r,c) = (fromIntegral $ c - 1, fromIntegral $ 1 - r)
> renderResult _ = mempty

> renderPlayer X = x
> renderPlayer O = o

> xMap = renderSolvedP . solveFor X $ gameTree
> oMap = renderOtherP  . solveFor O $ gameTree

> example = pad 1.1 xMap # scale 100
> -- pad 1.1 oMap # scale 100
