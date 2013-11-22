Inspired by [Randall Munroe](http://xkcd.com/832/), here are some
handy guides to optimal tic-tac-toe play, created with the [diagrams
EDSL](http://byorgey.wordpress.com/2011/05/17/announcing-diagrams-preview-release/).
Click the images to open larger PDF versions.

<a href="http://byorgey.files.wordpress.com/2011/05/xmap.pdf"><img src="http://byorgey.files.wordpress.com/2011/05/xmap.png" alt="" title="xMap" width="400" height="400" class="aligncenter size-full wp-image-599" /></a>

<a href="http://byorgey.files.wordpress.com/2011/05/omap.pdf"><img src="http://byorgey.files.wordpress.com/2011/05/omap.png" alt="" title="oMap" width="400" height="400" class="aligncenter size-full wp-image-601" /></a>

I hacked this up in just a few hours.  How did I do it?  First, some
code for solving tic-tac-toe (no graphics involved here, just game
trees and minimax search):

> {-# LANGUAGE PatternGuards, ViewPatterns #-}
> 
> module Solve where
> 
> import Data.Array
> import Data.Tree
> import Data.Function (on)
> import Data.List (groupBy, maximumBy)
> import Data.Maybe (isNothing, isJust)
> import Data.Monoid
> import Control.Applicative (liftA2)
> import Control.Arrow ((&&&))
> import Control.Monad (guard)
> 
> data Player = X | O
>   deriving (Show, Eq, Ord)
> 
> next X = O
> next O = X
> 
> data Result = Win Player Int [Loc] -- ^ This player can win in n moves
>             | Cats                 -- ^ Tie game
>   deriving (Show, Eq)
> 
> compareResultsFor :: Player -> (Result -> Result -> Ordering)
> compareResultsFor X = compare `on` resultToScore
>     where resultToScore (Win X n _) = (1/(1+fromIntegral n))
>           resultToScore Cats        = 0
>           resultToScore (Win O n _) = (-1/(1+fromIntegral n))
> compareResultsFor O = flip (compareResultsFor X)
> 
> type Loc = (Int,Int)
> type Board = Array Loc (Maybe Player)
> 
> emptyBoard :: Board
> emptyBoard = listArray ((0,0), (2,2)) (repeat Nothing)
> 
> showBoard :: Board -> String
> showBoard = unlines . map showRow 
>           . groupBy ((==) `on` (fst . fst)) . assocs
>   where showRow = concatMap (showPiece . snd)
>         showPiece Nothing  = " "
>         showPiece (Just p) = show p
> 
> data Move = Move Player Loc
>   deriving Show
> 
> makeMove :: Move -> Board -> Board
> makeMove (Move p l) b = b // [(l, Just p)]
> 
> data Game = Game Board           -- ^ The current board state.
>                  Player          -- ^ Whose turn is it?
>                  [Move]          -- ^ The list of moves so far (most
>                                  --   recent first).
>   deriving Show
> 
> initialGame = Game emptyBoard X []
> 
> -- | The full game tree for tic-tac-toe.
> gameTree :: Tree Game
> gameTree = unfoldTree (id &&& genMoves) initialGame
> 
> -- | Generate all possible successor games from the given game.
> genMoves :: Game -> [Game]
> genMoves (Game board player moves) = newGames
>   where validLocs = map fst . filter (isNothing . snd) . assocs $ board
>         newGames  = [Game (makeMove m board) (next player) (m:moves)
>                       | p <- validLocs
>                       , let m = Move player p
>                     ]
> 
> -- | Simple fold for Trees.  The Data.Tree module does not provide
> --   this.
> foldTree :: (a -> [b] -> b) -> Tree a -> b
> foldTree f (Node a ts) = f a (map (foldTree f) ts)
> 
> -- | Solve the game for player @p@: prune all but the optimal moves
> --   for player @p@, and annotate each game with its result (given
> --   best play).
> solveFor :: Player -> Tree Game -> Tree (Game, Result)
> solveFor p = foldTree (solveStep p)
> 
> -- | Given a game and its continuations (including their results),
> --   solve the game for player p.  If it is player p's turn, prune all
> --   continuations except the optimal one for p. Otherwise, leave all
> --   continuations.  The result of this game is the result of the
> --   optimal choice if it is p's turn, otherwise the worst possible
> --   outcome for p.
> solveStep :: Player -> Game -> [Tree (Game, Result)] -> Tree (Game, Result)
> solveStep p g@(Game brd curPlayer moves) conts
>   | Just res <- gameOver g = Node (g, res) []
> 
>   | curPlayer == p = let c   = bestContFor p conts
>                          res = inc . snd . rootLabel $ c
>                      in  Node (g, res) [c]
>   | otherwise      = Node (g, bestResultFor (next p) conts) conts
> 
> bestContFor :: Player -> [Tree (Game, Result)] -> Tree (Game, Result)
> bestContFor p = maximumBy (compareResultsFor p `on` (snd . rootLabel))
> 
> bestResultFor :: Player -> [Tree (Game, Result)] -> Result
> bestResultFor p = inc . snd . rootLabel . bestContFor p
> 
> inc :: Result -> Result
> inc (Win p n ls) = Win p (n+1) ls
> inc Cats         = Cats
> 
> -- | Check whether the game is over, returning the result if it is.
> gameOver :: Game -> Maybe Result
> gameOver (Game board _ _)
>   = getFirst $ mconcat (map (checkWin board) threes) <> checkCats board
> 
> checkWin :: Board -> [Loc] -> First Result
> checkWin board = First
>                . (>>= winAsResult)      -- Maybe Result
>                . mapM strength          -- Maybe [(Loc, Player)]
>                . map (id &&& (board!))  -- [(Loc, Maybe Player)]
> 
> winAsResult :: [(Loc, Player)] -> Maybe Result
> winAsResult (unzip -> (ls,ps))
>   | Just p <- allEqual ps = Just (Win p 0 ls)
> winAsResult _ = Nothing
> 
> checkCats :: Board -> First Result
> checkCats b | all isJust (elems b) = First (Just Cats)
>             | otherwise            = First Nothing
> 
> allEqual :: Eq a => [a] -> Maybe a
> allEqual = foldr1 combine . map Just
>   where combine (Just x) (Just y) | x == y = Just x
>                                   | otherwise = Nothing
>         combine Nothing _         = Nothing
>         combine _ Nothing         = Nothing
> 
> strength :: Functor f => (a, f b) -> f (a,b)
> strength (a, f) = fmap ((,) a) f
> 
> threes :: [[Loc]]
> threes = rows ++ cols ++ diags
>   where rows     = [ [ (r,c) | c <- [0..2] ] | r <- [0..2] ]
>         cols     = [ [ (r,c) | r <- [0..2] ] | c <- [0..2] ]
>         diags    = [ [ (i,i) | i <- [0..2] ]
>                    , [ (i,2-i) | i <- [0..2] ]
>                    ]

Once we have a solved game tree, we can use it to generate a graphical
map as follows.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> 
> -- Maps of optimal tic-tac-toe play, inspired by similar maps created
> -- by Randall Munroe, http://xkcd.com/832/
> 
> import Diagrams.Prelude hiding (Result)
> import Diagrams.Backend.Cairo.CmdLine
> 
> import Data.List.Split (chunk)                   -- cabal install split
> import Data.Maybe (fromMaybe, catMaybes)
> import qualified Data.Map as M
> import Data.Tree
> import Control.Arrow (second, (&&&), (***))
> import Data.Array (assocs)
> 
> import Solve
> 
> type D = Diagram Cairo R2
> 
> x, o :: D
> x = (stroke $ fromVertices [P (-1,1), P (1,-1)] 
>               <> fromVertices [P (1,1), P (-1,-1)])
>   # lw 0.05
>   # lineCap LineCapRound
>   # scale 0.4
>   # freeze
>   # centerXY
> o = circle
>   # lw 0.05
>   # scale 0.4
>   # freeze
> 
> -- | Render a list of lists of diagrams in a grid.
> grid :: Double -> [[D]] -> D
> grid s = centerXY
>        . vcat' with {catMethod = Distrib, sep = s}
>        . map (hcat' with {catMethod = Distrib, sep = s})
> 
> -- | Given a mapping from (r,c) locations (in a 3x3 grid) to diagrams,
> --   render them in a grid, surrounded by a square.
> renderGrid :: M.Map Loc D -> D
> renderGrid g
>   = (grid 1
>   . chunk 3
>   . map (fromMaybe (phantom x) . flip M.lookup g)
>   $ [ (r,c) | r <- [0..2], c <- [0..2] ])
> 
>     `atop`
>     square # lw 0.02 # scale 3 # freeze
> 
> -- | Given a solved game tree, where the first move is being made by
> --   the player for whom the tree is solved, render a map of optimal play.
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
> 
> -- | Given a solved game tree, where the first move is being made by the
> --   opponent of the player for whom the tree is solved, render a map of optimal
> --   play.
> renderOtherP :: Tree (Game, Result) -> D
> renderOtherP (Node _ conts)
>     -- just recursively render each game arising from an opponent's move in a grid.
>   = renderGrid . M.fromList . map (getMove &&& (scale (1/3) . renderSolvedP)) $ conts
>   where getMove (Node (Game _ _ (Move _ m : _), _) _) = m
> 
> -- | Extract the current moves from a board.
> curMoves :: Board -> M.Map Loc D
> curMoves = M.fromList . (map . second) renderPlayer . catMaybes . map strength . assocs
> 
> -- | Render a line through a win.
> renderResult :: Result -> D
> renderResult (Win _ 0 ls) = winLine # freeze
>   where winLine :: D
>         winLine = stroke (fromVertices (map (P . conv) ls))
>                           # lw 0.2
>                           # lc blue
>                           # lineCap LineCapRound
>         conv (r,c) = (fromIntegral $ c - 1, fromIntegral $ 1 - r)
> renderResult _ = mempty
> 
> renderPlayer X = x
> renderPlayer O = o
> 
> xMap = renderSolvedP . solveFor X $ gameTree
> oMap = renderOtherP  . solveFor O $ gameTree
> 
> main = defaultMain (pad 1.1 xMap)
>        -- defaultMain (pad 1.1 oMap)