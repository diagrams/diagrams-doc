{-# LANGUAGE PatternGuards, ViewPatterns #-}

module TicTacToeSolve where

import Data.Array
import Data.Tree
import Data.Function (on)
import Data.List (groupBy, maximumBy)
import Data.Maybe (isNothing, isJust)
import Data.Monoid
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad (guard)

data Player = X | O
  deriving (Show, Eq, Ord)

next X = O
next O = X

data Result = Win Player Int [Loc] -- ^ This player can win in n moves
            | Cats                 -- ^ Tie game
  deriving (Show, Eq)

compareResultsFor :: Player -> (Result -> Result -> Ordering)
compareResultsFor X = compare `on` resultToScore
    where resultToScore (Win X n _) = (1/(1+fromIntegral n))
          resultToScore Cats        = 0
          resultToScore (Win O n _) = (-1/(1+fromIntegral n))
compareResultsFor O = flip (compareResultsFor X)

type Loc = (Int,Int)
type Board = Array Loc (Maybe Player)

emptyBoard :: Board
emptyBoard = listArray ((0,0), (2,2)) (repeat Nothing)

showBoard :: Board -> String
showBoard = unlines . map showRow . groupBy ((==) `on` (fst . fst)) . assocs
  where showRow = concatMap (showPiece . snd)
        showPiece Nothing  = " "
        showPiece (Just p) = show p

data Move = Move Player Loc
  deriving Show

makeMove :: Move -> Board -> Board
makeMove (Move p l) b = b // [(l, Just p)]

data Game = Game Board           -- ^ The current board state.
                 Player          -- ^ Whose turn is it?
                 [Move]          -- ^ The list of moves so far (most
                                 --   recent first).
  deriving Show

initialGame = Game emptyBoard X []

-- | The full game tree for tic-tac-toe.
gameTree :: Tree Game
gameTree = unfoldTree (id &&& genMoves) initialGame

-- | Generate all possible successor games from the given game.
genMoves :: Game -> [Game]
genMoves (Game board player moves) = newGames
  where validLocs = map fst . filter (isNothing . snd) . assocs $ board
        newGames  = [Game (makeMove m board) (next player) (m:moves)
                      | p <- validLocs
                      , let m = Move player p
                    ]

-- | Simple fold for Trees.  The Data.Tree module does not provide
--   this.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)

-- | Solve the game for player @p@: prune all but the optimal moves
--   for player @p@, and annotate each game with its result (given
--   best play).
solveFor :: Player -> Tree Game -> Tree (Game, Result)
solveFor p = foldTree (solveStep p)

-- | Given a game and its continuations (including their results),
--   solve the game for player p.  If it is player p's turn, prune all
--   continuations except the optimal one for p. Otherwise, leave all
--   continuations.  The result of this game is the result of the
--   optimal choice if it is p's turn, otherwise the worst possible
--   outcome for p.
solveStep :: Player -> Game -> [Tree (Game, Result)] -> Tree (Game, Result)
solveStep p g@(Game brd curPlayer moves) conts
  | Just res <- gameOver g = Node (g, res) []

  | curPlayer == p = let c   = bestContFor p conts
                         res = inc . snd . rootLabel $ c
                     in  Node (g, res) [c]
  | otherwise      = Node (g, bestResultFor (next p) conts) conts

bestContFor :: Player -> [Tree (Game, Result)] -> Tree (Game, Result)
bestContFor p = maximumBy (compareResultsFor p `on` (snd . rootLabel))

bestResultFor :: Player -> [Tree (Game, Result)] -> Result
bestResultFor p = inc . snd . rootLabel . bestContFor p

inc :: Result -> Result
inc (Win p n ls) = Win p (n+1) ls
inc Cats         = Cats

-- | Check whether the game is over, returning the result if it is.
gameOver :: Game -> Maybe Result
gameOver (Game board _ _)
  = getFirst $ mconcat (map (checkWin board) threes) `mappend` checkCats board

checkWin :: Board -> [Loc] -> First Result
checkWin board = First
               . (>>= winAsResult)      -- Maybe Result
               . mapM strength          -- Maybe [(Loc, Player)]
               . map (id &&& (board!))  -- [(Loc, Maybe Player)]

winAsResult :: [(Loc, Player)] -> Maybe Result
winAsResult (unzip -> (ls,ps))
  | Just p <- allEqual ps = Just (Win p 0 ls)
winAsResult _ = Nothing

checkCats :: Board -> First Result
checkCats b | all isJust (elems b) = First (Just Cats)
            | otherwise            = First Nothing

allEqual :: Eq a => [a] -> Maybe a
allEqual = foldr1 combine . map Just
  where combine (Just x) (Just y) | x == y = Just x
                                  | otherwise = Nothing
        combine Nothing _         = Nothing
        combine _ Nothing         = Nothing

strength :: Functor f => (a, f b) -> f (a,b)
strength (a, f) = fmap ((,) a) f

threes :: [[Loc]]
threes = rows ++ cols ++ diags
  where rows     = [ [ (r,c) | c <- [0..2] ] | r <- [0..2] ]
        cols     = [ [ (r,c) | r <- [0..2] ] | c <- [0..2] ]
        diags    = [ [ (i,i) | i <- [0..2] ]
                   , [ (i,2-i) | i <- [0..2] ]
                   ]
