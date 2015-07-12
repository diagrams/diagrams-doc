%% -*- mode: LaTeX; compile-command: "runhaskell Shake && open abstract.pdf" -*-

\documentclass[9pt,authoryear,nocopyrightspace]{sigplanconf}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lhs2TeX

%include polycode.fmt

% Use 'arrayhs' mode, so code blocks will not be split across page breaks.
%\arrayhs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Package imports

\usepackage{amsthm}
\usepackage{amsmath}
\usepackage{mathtools}
\usepackage{latexsym}
\usepackage{amssymb}
\usepackage{stmaryrd}
\usepackage{comment}
\usepackage{url}
\usepackage{xspace}
\usepackage{xcolor}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Page size

\pdfpagewidth=8.5in
\pdfpageheight=11in

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Diagrams

\usepackage{graphicx}
\usepackage[outputdir=diagrams/]{diagrams-latex}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prettyref

\usepackage{prettyref}

\newrefformat{fig}{Figure~\ref{#1}}
\newrefformat{sec}{\sect\ref{#1}}
\newrefformat{eq}{equation~\eqref{#1}}
\newrefformat{prob}{Problem~\ref{#1}}
\newrefformat{tab}{Table~\ref{#1}}
\newrefformat{thm}{Theorem~\ref{#1}}
\newrefformat{lem}{Lemma~\ref{#1}}
\newrefformat{prop}{Proposition~\ref{#1}}
\newrefformat{defn}{Definition~\ref{#1}}
\newrefformat{cor}{Corollary~\ref{#1}}
\newcommand{\pref}[1]{\prettyref{#1}}

% \Pref is just like \pref but it uppercases the first letter; for use
% at the beginning of a sentence.
\newcommand{\Pref}[1]{%
  \expandafter\ifx\csname r@@#1\endcsname\relax {\scriptsize[ref]}
    \else
    \edef\reftext{\prettyref{#1}}\expandafter\MakeUppercase\reftext
    \fi
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comments

% big, top-level (verbatim) comments

\specialcomment{todoP}{\begingroup\color{red}TODO: }{\endgroup}

% quick (inline) comments

\newif\ifcomments\commentstrue

\ifcomments
\newcommand{\authornote}[3]{\textcolor{#1}{[#3 ---#2]}}
\newcommand{\todo}[1]{\textcolor{red}{[TODO: #1]}}
\else
\newcommand{\authornote}[3]{}
\newcommand{\todo}[1]{}
\fi

\newcommand{\bay}[1]{\authornote{blue}{BAY}{#1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Semantic markup

\newcommand{\eg}{\emph{e.g.}\xspace}
\newcommand{\ie}{\emph{i.e.}\xspace}
\newcommand{\etal}{\emph{et al.}\xspace}

\newcommand{\term}[1]{\emph{#1}}

\newcommand{\pkg}[1]{\textsf{#1}}

\newcommand{\diagrams}{\pkg{diagrams}\xspace}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

%\thispagestyle{empty}

\title{Diagrams---A Functional EDSL for Vector Graphics}
\subtitle{Demo proposal}

\authorinfo{Ryan Yates}
{Department of Computer Science \\ University of Rochester \\
  Rochester, New York, USA}
{ryates@@cs.rochester.edu}

\authorinfo{Brent A. Yorgey}
{Dept. of Mathematics and Computer Science\\ Hendrix College\\
Conway, Arkansas, USA}
{byorgey@@gmail.com}

\preprintfooter{Submitted to FARM 2015}

\conferenceinfo{FARM '15}{September 5, 2015, Vancouver, British
  Columbia, Canada}
\copyrightyear{2015}

\maketitle

% Here's the relevant portion of the FARM CFP:
%
% Demo abstracts should describe the demonstration and its context,
% connecting it with the themes of FARM. A demo could be in the form
% of a short (10-20 minute) tutorial, presentation of
% work-in-progress, an exhibition of some work, or even a
% performance. Abstracts should be no longer than 2 pages, using the
% ACM SIGPLAN template and will be subject to a light-touch peer
% review.

\section{Introduction}

\diagrams\footnote{http://projects.haskell.org/diagrams} is a
declarative domain-specific language for creating vector graphics,
embedded in the Haskell programming language~\cite{haskell}.  Under
continuous development for the past 4+ years, it serves as a powerful
platform for creating illustrations, visualizations, and artwork, as
well as a testbed for new ideas in functional EDSLs and in functional
approaches to graphics.  Designed with ``power users'' in mind, it
includes support for multiple vector spaces, pluggable rendering
backends, a full range of standard drawing attributes, sophisticated
algorithms for working with paths, and support for relative
positioning of the constituent parts of a diagram.  It makes extensive
use of Haskell's type system to capture geometric invariants, and uses
a pure functional paradigm both in its internal design (for example,
using first-class functions to represent information about boundaries)
as well as in the design of its API, which emphasizes composition
rather than mutation.

We will give a short \diagrams tutorial/demo, first explaining just
enough of the basics to get started, and then using the remainder of
the time to show off some more sophisticated examples.  In what
follows, we include a few representative examples, with commentary
explaining what features of the framework are illustrated by each
example, and the particular ways in which the examples highlight the
power of a functional EDSL \cite{hudak1996building}.

\section{Examples}
\label{sec:examples}

\pref{fig:hilbert} shows an order-$5$ fractal Hilbert curve
\cite{hilbert1891ueber}, along with the complete code used to generate
it.  Of course, recursive functions such as |hilbert| are the bread
and butter of functional programming.  This example also shows off the
compositional nature of the framework, in this case building up a
complex \emph{path} by concatenating shorter paths using the |<>|
operator.  In fact, |<>| denotes not just concatenation of paths, but
more generally the associative combining operation for any
\emph{monoid}---of which \diagrams has quite a few, including paths,
colors, transformations, styles, and diagrams themselves \cite{yorgey2012monoids}.

\begin{figure}
  \centering
  \begin{diagram}[width=150]
hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

dia = hilbert 5 # strokeT
    # lc darkred # lw medium # frame 1
  \end{diagram}

\begin{verbatim}
hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

dia = hilbert 5 # strokeT
    # lc darkred # lw medium # frame 1
\end{verbatim}
  \caption{Order-5 Hilbert curve, with code}
  \label{fig:hilbert}
\end{figure}

\pref{fig:tree} shows a binary tree, with different types of nodes at
its leaves, along with the complete code used to generate it
\cite{piponipolynomial}.  The first few lines define |t|, an abstract
representation of the tree to be drawn, and the rest of the lines
specify how to render it.  This example illustrates the ability of an
\emph{embedded} DSL to leverage the abstraction facilities of its host
language: here we define a new data type, |LeafType|, and use it to
precisely enumerate the possibilities for leaves in the tree to be
drawn, and define functions to abstract out common patterns (|nd|,
|lf|) and to specify custom behavior (|drawType|).  We also make use
of higher-order functions: |map| is higher-order, of course, but more
interestingly, so is |renderTree|, which takes function arguments
specifying how to draw nodes and edges of a tree.  Finally, this
example shows off the fact that a standard installation of \diagrams
comes with ``batteries included'', such as the tree layout algorithm
used here.

\begin{figure}
\begin{center}
\begin{diagram}[width=150]
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import Data.Char (toLower)

data LeafType = A || B || H  deriving Show

t = nd [ nd [ nd $ map lf [B, B], lf B ]
       , nd [ nd [ lf H, nd $ map lf [A, A] ]
            , nd $ map lf [A, A]
            ]
       ]
  where nd     = Node Nothing
        lf x   = Node (Just x) []

drawType x = mconcat
  [ text (map toLower (show x)) # italic # centerX
  , drawNode x ]

drawNode A = square 2 # fc yellow
drawNode B = circle 1 # fc red
drawNode H = circle 1 # fc white # dashingG [0.2,0.2] 0

renderT :: Tree (Maybe LeafType) -> Diagram B
renderT
  = renderTree (maybe mempty drawType) (~~)
  . symmLayout' (with & slHSep .~ 4 & slVSep .~ 3)

dia = renderT t # frame 0.5
\end{diagram}
%$
\begin{verbatim}
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import Data.Char (toLower)

data LeafType = A | B | H  deriving Show

t = nd [ nd [ nd $ map lf [B, B], lf B ]
       , nd [ nd [ lf H, nd $ map lf [A, A] ]
            , nd $ map lf [A, A]
            ]
       ]
  where nd     = Node Nothing
        lf x   = Node (Just x) []

drawType x = mconcat
  [ text (map toLower (show x)) # italic # centerX
  , drawNode x ]

drawNode A = square 2 # fc yellow
drawNode B = circle 1 # fc red
drawNode H = circle 1 # fc white
                      # dashingG [0.2,0.2] 0

renderT :: Tree (Maybe LeafType) -> Diagram B
renderT
  = renderTree (maybe mempty drawType) (~~)
  . symmLayout' (with & slHSep .~ 4 & slVSep .~ 3)

dia = renderT t # frame 0.5
\end{verbatim}
\end{center}
\caption{Labelled binary tree, with code} \label{fig:tree}
\end{figure}

Finally, \pref{fig:bwt} shows a portrait of the Burrows--Wheeler
transform (BWT) \cite{burrows1994block} that was included in the Bridges
mathematical art exhibition at the 2014 Joint Mathematics Meetings.
This is a portrait in the sense that it captures an aspect of an algorithm
concretely.  In the middle on the left side an input value starts
the diagram.  This value is manipulated according to the steps of
BWT clockwise with the top half encoding and the bottom half decoding
back to the original inpnut.

Having the full expressiveness of Haskell helped to shape this work as it was
created.  Processes like extracting common code and generalizing functions
allowed rapid exploration of visual patterns and the development of a visual
language for the work.  For instance, the |alphabet| function produces a
diagram of nested circles for a given number.  Originally it had the colors
``baked in'', but later, when connecting parts of the diagram, Haskell's ease of
refactoring allowed the extraction of a function with the colors with little
effort.  Other shapes and even variable sized shapes could easily and rapidly be
experimented with as the image was being constructed.

The flexibility of \diagrams also allowed the exploration of various
compositions with little change to the code.  Instead of blocks proceeding
clockwise, we could have a single linear progression, or a radial layout that
fanned out like a circle.  Most of these variations could be explored
with small changes to the code responsible for composition.  Indeed, various
layouts were revisited later in the process even with significant changes in
the code by keeping the other layouts around and fixing errors caught by type
checking.  In the end the algorithm's own transition from
row to column gives an opportunity for the arrangement around a square and
a reflective symmetry of sorts across the horizontal midline.

Although the full code is too long to include, a small excerpt is shown
which illustrates building up part of the diagram as a composition of
other diagrams.

\begin{figure}
\begin{center}
\begin{diagram}[width=250]
import Data.Char
import Data.List hiding (sort)
import Data.Maybe

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
    leq k (x:xs) (y:ys) = x < y |||| (x == y && leq (k-1) xs ys)

recreate :: Ord a => Int -> [a] -> [[a]]
recreate 0 ys = map (const []) ys
recreate k ys = sortby leq (join ys (recreate (k-1) ys))
  where leq us vs = head us <= head vs
        join xs xss = [y:ys || (y,ys) <- zip xs xss]

unbwt :: Ord a => Int -> [a] -> [a]
unbwt t ys = take (length ys) (thread (spl ys t))
  where thread (x,j) = x:thread (spl ys j)
        spl ys t = fromJust $ lookup t (zip [1..] (sortby (<=) (zip ys [1..])))

-----------
alphabet' :: Double -> Int -> Diagram B
alphabet' w (-1) = square 1 # lc red # lwG w # withEnvelope (circle 1 :: Diagram B)
alphabet' w i    = c # lc (acolor i) # lwG w
  where
    m  = abs i `mod` 10
    c  = mconcat [circle (fromIntegral (m + 1 - r) / fromIntegral (m + 1)) || r <- [0..m]]

alphabet = alphabet' 0.05

acolor :: Int -> Colour Double
acolor (-1) = red
acolor i = cs !! (abs i `mod` l)
  where
    l  = length cs
    cs = [red, orange, yellow, green, blue, purple]

d :: Diagram B
d = squared
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
                                                           # rotate (1/2 @@@@ turn)

    inputToBWT =
      [ block rs # reflectX    -- Rotations of s
      , sorting 7 head rs rs'
      , block rs'              -- Sorted rotations
                               -- of s
      ]
      # hcatSepCenter hsep

    buildUnbwt =
      [ block [[a,b] || (a,b) <- ps] # reflectX  -- spl table
      , sorting 23.1 fst ps ps'
      , block [[a,b,i] || (i,(a,b)) <- ips]      -- continued
      , mconcat [ (0 ^& 0) ~~ ((7-(2+2*hsep)+fromIntegral j * (2+hsep)) ^& 0)
                    # lc (acolor x)
                    # withEnvelope (strutY 2 :: Diagram B)
                    # lwG 0.05
                    # moveTo (0 ^& (fromIntegral (length p - i) * (2+vsep)))
                || (j,(i,x,_)) <- zip [1..] ts
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
                          || (i,x,j) <- ts
                          ]

    row = hcatSep hsep
    block = vhcatSep hsep vsep . map (map alphabet)

    sorting w f rs rs' = reflectY $ mconcat
            [ connectH w i j # lc (acolor (f r))
            || (i,r) <- zip [0 :: Int ..] rs
            , let j = fromJust . findIndex (== r) $ rs'
            ]
    connectH w i j = bez (0 ^& f i) (w*2/5 ^& f i) (w*3/5 ^& f j) (w ^& f j) # lwG 0.05
      where
        f x = fromIntegral x * (2+vsep)

    connectW w i j
      || abs (i - j) < 2 = strutX w
      || otherwise
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

dia = d # centerXY # pad 1.1
\end{diagram}
%$
\begin{verbatim}
...
    inputToBWT =
      [ block rs # reflectX    -- Rotations of s
      , sorting 7 head rs rs'
      , block rs'              -- Sorted rotations
                               -- of s
      ]
      # map centerXY
      # hcat' (with & sep .~ 0.1)
...
\end{verbatim}
\end{center}
\caption{A portrait of the Burrows--Wheeler transform.  The small code fragment is
the top portion of the image.} \label{fig:bwt}
\end{figure}

\bibliographystyle{plainnat}
\bibliography{abstract}

\end{document}

