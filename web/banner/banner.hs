hilbert :: [Diagram B]
hilbert = iterate expand mempty
  where
    expand t =
      let u = vcat [ t, alignT (vrule 1)
                   , rotateBy (3/4) t ]
      in      hcat [ u, hrule 1, reflectX u ]
                   # alignBL

diagram :: Diagram B
diagram = hilbert !! 6
