hilbert 0 = mempty
hilbert n = hilbert (n-1) # rotateBy (1/4) # reflectY
         <> vrule 1
         <> hilbert (n-1)
         <> hrule 1
         <> hilbert (n-1)
         <> vrule (-1)
         <> hilbert (n-1) # rotateBy (1/4) # reflectX

diagram :: Diagram B
diagram = lc silver . opacity 0.3 . strokeT $ hilbert 6
