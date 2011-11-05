> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
>
> hilbert :: [Trail R2]
> hilbert = iterate genHilbert mempty
>   where genHilbert t = let t' = reverseTrail t
>                        in  mconcat [ rotateBy (-1/4) t'
>                                    , fromOffsets [unitY]
>                                    , t
>                                    , fromOffsets [unitX]
>                                    , t
>                                    , fromOffsets [negateV unitY]
>                                    , rotateBy (1/4) t'
>                                    ]
>
> example = (hilbert !! 6)
>   # strokeT
>   # lw 0.2
>   # fc red
>   # centerXY
>   # pad 1.1
