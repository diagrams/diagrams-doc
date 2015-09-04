From <https://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/>

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module BoltzmannTrees where
>
> import           Control.Applicative
> import           Control.Lens                   ((??))
> import           Control.Monad.Random
> import           Control.Monad.Reader
> import           Control.Monad.State
> import           Control.Monad.Trans.Maybe

So here’s a simple type of binary tree shapes, containing no data:

> data Tree a = Leaf a | Branch a (Tree a) (Tree a)
>   deriving Show
>
> type Tree' = Tree ()

We’ll count each constructor (Leaf or Branch) as having a size of 1:

> size :: Tree a -> Int
> size (Leaf _) = 1
> size (Branch _ l r) = 1 + size l + size r

Now, suppose we want to randomly generate these trees. This is an entirely
reasonable and useful thing to do: perhaps we want to, say, randomly test
properties of functions over Tree using QuickCheck. Here’s the simplest, most
naïve way to do it:

> randomTree :: (Applicative m, MonadRandom m) => m Tree'
> randomTree = do
>   r <- getRandom
>   if r < (1/2 :: Double)
>     then return $ Leaf ()
>     else Branch <$> pure () <*> randomTree <*> randomTree

We choose each of the constructors with probability 1/2, and recurse in the
Branch case.

Now, as is well-known, this works rather poorly. Why is that? Let’s generate
100 random trees and print out their sizes in descending order:

ghci> reverse . sort . map size <$> replicateM 100 randomTree
[118331,7753,2783,763,237,203,195,163,159,73,65,63,49,41,39,29,29,23,23,21,
19,19,15,11,9,9,9,9,7,7,7,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3,3,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1]

As you can see, this is a really weird distribution of sizes. For one thing, we
get lots of trees that are very small—in fact, it’s easy to see that we expect
about 50 of them to be single leaf nodes. The other weird thing, however, is
that we also get some really humongous trees. The above output gets randomly
regenerated every time I process this post—so I don’t know exactly what sizes
you’ll end up seeing—but it’s a good bet that there is at least one tree with a
size greater than 10^4. To get an intuitive idea of why this happens, imagine
generating the tree in a breadth-first manner. At each new level we have a
collection of “active” nodes corresponding to pending recursive calls to
randomTree. Each active node generates zero or two new active nodes on the next
level with equal probability, so on average the number of active nodes remains
the same from level to level. So if we happen to make a lot of Branch choices
right off the bat, it may take a long time before the tree “thins out” again.
And if this distribution didn’t seem weird enough already, it turns out (though
it is far from obvious how to prove this) that the expected size of the
generated trees is infinite!

The usual solution with QuickCheck is to use the sized combinator to limit the
size of generated structures, but this does not help with the problem of having
too many very small trees.

Here’s a (seemingly!) stupid idea. Suppose we want to generate trees of size
approximately 100 (say, within 10%). Let’s simply use the above algorithm, but
with the following modifications:

If we generate a tree of size < 90, throw it away and start over.  If we
generate a tree of size > 110, throw it away and start over. As an
optimization, however, we will stop as soon as the size goes over 110; that is,
we will keep track of the current size while generating and stop early if the
size gets too big.

Here’s some code. First, a monad onion:

> newtype GenM a = GenM 
>     { unGenM :: ReaderT (Int,Int) (StateT Int (MaybeT (Rand StdGen))) a }
>   deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadRandom,
>             MonadState Int, MonadReader (Int,Int))

The ReaderT holds the min and max allowed sizes; the StateT holds the current
size; the MaybeT allows for possible failure (if the tree gets too big or ends
up too small), and the Rand StdGen is, of course, for generating random
numbers. To run a computation in this monad we take a target size and a
tolerance and use them to compute minimum and maximum sizes. (The (??) in the
code below is an infix version of flip, defined in the lens package.)

> runGenM' :: StdGen -> Int -> Double -> GenM a -> Maybe a
> runGenM' g targetSize eps m =
>   let wiggle  = floor $ fromIntegral targetSize * eps
>       minSize = targetSize - wiggle
>       maxSize = targetSize + wiggle
>   in
>       (evalRand ?? g) . runMaybeT . (evalStateT ?? 0)
>       . (runReaderT ?? (minSize, maxSize)) . unGenM
>       $ m
>
> runGenM :: Int -> Double -> GenM a -> IO (Maybe a)
> runGenM targetSize eps m = do
>   g <- newStdGen
>   return $ runGenM' g targetSize eps m
>
> seedGenM :: Int -> Int -> Double -> GenM a -> Maybe a
> seedGenM = runGenM' . mkStdGen

Here’s the code to try generating a tree: we call the atom function to record
the increase in size, and choose between the two constructors with equal
probability. atom, in turn, handles failing early if the size gets too big.

> genTreeUB :: GenM Tree'
> genTreeUB = do
>   r <- getRandom
>   atom
>   if r <= (1/2 :: Double)
>     then return $ Leaf ()
>     else Branch <$> pure () <*> genTreeUB <*> genTreeUB
> 
> atom :: GenM ()
> atom = do
>   (_, maxSize) <- ask
>   curSize <- get
>   when (curSize >= maxSize) mzero
>   put (curSize + 1)

genTreeLB calls genTreeUB and then performs the lower bound check on the size.

> genTreeLB :: GenM Tree'
> genTreeLB = do
>   put 0
>   t <- genTreeUB
>   tSize <- get
>   (minSize, _) <- ask
>   guard $ tSize >= minSize
>   return t

Finally, genTree just calls genTreeLB repeatedly until it succeeds.

> genTree :: GenM Tree'
> genTree = genTreeLB `mplus` genTree

Let’s make sure it works:

ghci> map size . fromJust <$> runGenM 100 0.1 (replicateM 30 genTree)
  [105,91,105,103,107,101,105,93,93,93,95,91,103,91,91,107,105,103,97,95,105,
    107,93,97,93,103,91,103,101,95]

Neat! Okay, but surely this is really, really slow, right? We spend a bunch of
time just throwing away trees of the wrong size. Before reading on, would you
care to guess the asymptotic time complexity to generate a tree of size n using
this algorithm?

And while you think about that, here is a random binary tree of size
approximately 1000.



And the answer is… it is linear! That is, it takes O(n) time to generate a tree
of size n. This is astounding—it’s the best we could possibly hope for, because
of course it takes at least O(n) time to generate an object of size O(n). If
you don’t believe me, I invite you to run some experiments with this code
yourself. I did, and it sure looks linear:

main = do
  [sz] <- getArgs
  Just ts <- runGenM (read sz) 0.1 $ replicateM 1000 genTree
  print . (/fromIntegral n) . fromIntegral . sum . map size $ ts

archimedes :: research/species/boltzmann » time ./GenTree 50
49.682
./GenTree 50  1.37s user 0.01s system 99% cpu 1.387 total
archimedes :: research/species/boltzmann » time ./GenTree 100
99.474
./GenTree 100  3.11s user 0.02s system 99% cpu 3.152 total
archimedes :: research/species/boltzmann » time ./GenTree 200
198.494
./GenTree 200  6.82s user 0.04s system 99% cpu 6.876 total
archimedes :: research/species/boltzmann » time ./GenTree 400
398.798
./GenTree 400  13.08s user 0.08s system 99% cpu 13.208 total
archimedes :: research/species/boltzmann » time ./GenTree 800
795.798
./GenTree 800  25.99s user 0.16s system 99% cpu 26.228 total

The proof of this astounding fact uses some complex analysis which I do not
understand; I wish I was joking. Of course, the constant factor can be big,
depending on how small you set the “epsilon” allowing for wiggle room around
the target size.[1] But it is still quite feasible to generate rather large trees
(with, say, 10^5 nodes).

There is much, much more to say on this topic. I just wanted to start out with
a simple example before jumping into more of the technical details and
generalizations, which I plan to write about in future posts. I also hope to
package this and a bunch of other stuff into a library. In the meantime, you
can read Duchon et. al [2] if you want the details.

Actually, if you set epsilon to zero, the asymptotic complexity jumps to
O(n^2).

Duchon, Philippe, et al. “Boltzmann samplers for the random generation of
combinatorial structures.” Combinatorics Probability and Computing 13.4-5
(2004): 577-625.

