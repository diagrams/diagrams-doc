Haskell logo in Diagrams by Ryan Yates.

Based on Metapost version Brian Sniffen.
This was based on a public domain PNG by Jeff Wheeler,
and on logo contest entries by George Pollard, Darrin Thompson, and others.

(c) 2009 Brian Sniffen, All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
may be used to endorse or promote products derived from this software
without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE. -}

> {-# LANGUAGE TypeFamilies #-}
> 
> import Diagrams.Prelude hiding (intersection,D)
> 
> import Diagrams.Backend.Cairo.CmdLine
> 
> import Data.VectorSpace
> import Data.Colour.SRGB
> 
> type D = Diagram Cairo R2
> 
> mkPath :: [R2] -> D
> mkPath = stroke . close . fromVertices . map P
> 
> rangle, lambdabody, lambdaleg, lambda :: Double -> Double -> D
> 
> rangle h m = mkPath
>   [ zeroV,       (4*h,4*m*h), (0,8*m*h)
>   , (3*h,8*m*h), (7*h,4*m*h), (3*h,0)
>   ]
> lambdabody h m = translate (4*h,0) (rangle h m)
> lambdaleg h m = translate (4*h,0) path
>   where path = mkPath [(11*h,0), (8*h,0), (0,8*m*h), (3*h,8*m*h)]
> 
> lambda h m = mkPath
>   [ (4*h,0),  (8*h,4*m*h), (4*h,8*m*h),     (7*h,8*m*h)
>   , (15*h,0), (12*h,0),    (9.5*h,2.5*m*h), (7*h,0)
>   ]

The equal sign is 5 units high: two units thick in each block, with
one unit thickness of white between.  Being centered, that puts its
outer corners at 3.5, 5.5, 6.5, and 8.5.  The right edge is at 17
units.  The left edge is the standard 1-unit horizontal gap from the
lambda.  These gaps are all by horizontal measure---the components are
closer than 1 unit to each other.

> equalsign :: Double -> Double -> Double -> Double -> D
> equalsign h m maxx miny = mkPath q
>   where
>     maxy = miny + 4/3;
>     cutoff = ((16*h,0*h), (8*h,8*m*h))
>     p1 = ((maxx*h,miny*m*h), (0*h,miny*m*h)) `intersection` cutoff;
>     p2 = ((maxx*h,maxy*m*h), (0*h,maxy*m*h)) `intersection` cutoff;
>     q  = [p1, p2, (maxx*h,maxy*m*h), (maxx*h,miny*m*h)]
> 
> stdrangle, stdlambda, stdlambdabody, stdlambdaleg, lowerequal, upperequal :: D
> 
> stdrangle     = rangle     1 (3/2)
> stdlambda     = lambda     1 (3/2)
> stdlambdabody = lambdabody 1 (3/2)
> stdlambdaleg  = lambdaleg  1 (3/2)
> lowerequal    = equalsign  1 (3/2) 17 (7/3)
> upperequal    = equalsign  1 (3/2) 17 (13/3)
> 
> example :: D
> example =     light   stdrangle
>     `atop` lighter stdlambda
>     `atop` light   lowerequal
>     `atop` light   upperequal
>   where light   = fc (sRGB 0.4 0.4 0.4)
>         lighter = fc (sRGB 0.6 0.6 0.6)

If we take the given vectors and put them in R^3 as (x,y,0), we are
resulting in the z component of their cross product.

> cross3Z :: (Num t) => (t, t) -> (t, t) -> t
> cross3Z (x0,y0) (x1,y1) = x0 * y1 - x1 * y0

This is just a one off version.  A more robust version would included
data indicating if the lines were parallel or colinear among other
things.

> intersection :: (t ~ Scalar t, Fractional t, VectorSpace t) =>
>      ((t, t), (t, t)) -> ((t, t), (t, t)) -> (t, t)
> intersection (a0,a1) (b0,b1) = a0 ^+^ (va ^* t)
>   where vb = b1 ^-^ b0
>         va = a1 ^-^ a0
>         v  = a0 ^-^ b0
>         d  = cross3Z va vb
>         n  = cross3Z vb v
>         t  = n / d
