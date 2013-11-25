%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

\usepackage{haskell}
%include lhs2TeX-extra.fmt

%format p1
%format p2
%format p3

\usepackage{brent}
\usepackage[backend=cairo,extension=pdf,outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}
\usepackage{ulem}

\renewcommand{\onelinecomment}{\quad--- \itshape}
\renewcommand{\Varid}[1]{{\mathit{#1}}}

% \setbeamertemplate{footline}{\insertframenumber}

\setbeamertemplate{items}[circle]

\mode<presentation>
{
  \usetheme{default}                          % use a default (plain) theme

  \setbeamertemplate{navigation symbols}{}    % don't show navigation
                                              % buttons along the
                                              % bottom
  \setbeamerfont{normal text}{family=\sffamily}

  % XX remove this before giving actual talk!
  % \setbeamertemplate{footline}[frame number]
  % {%
  %   \begin{beamercolorbox}{section in head/foot}
  %     \vskip2pt
  %     \hfill \insertframenumber
  %     \vskip2pt
  %   \end{beamercolorbox}
  % }

  \AtBeginSection[]
  {
    \begin{frame}<beamer>
      \frametitle{}

      \begin{center}
        % \includegraphics[width=1in]{\sectionimg}
        % \bigskip

        {\Huge \insertsectionhead}
      \end{center}
    \end{frame}
  }
}

\defbeamertemplate*{title page}{customized}[1][]
{
  \vbox{}
  \vfill
  \begin{centering}
    \begin{beamercolorbox}[sep=8pt,center,#1]{title}
      \usebeamerfont{title}\inserttitle\par%
      \ifx\insertsubtitle\@@empty%
      \else%
        \vskip0.25em%
        {\usebeamerfont{subtitle}\usebeamercolor[fg]{subtitle}\insertsubtitle\par}%
      \fi%
    \end{beamercolorbox}%
    \vskip1em\par
    {\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic\par}
    \vskip1em\par
    \begin{beamercolorbox}[sep=8pt,center,#1]{author}
      \usebeamerfont{author}\insertauthor
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{institute}
      \usebeamerfont{institute}\insertinstitute
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{date}
      \usebeamerfont{date}\insertdate
    \end{beamercolorbox}
  \end{centering}
  \vfill
}

% uncomment me to get 4 slides per page for printing
% \usepackage{pgfpages}
% \pgfpagesuselayout{4 on 1}[uspaper, border shrink=5mm]

% \setbeameroption{show only notes}

\renewcommand{\emph}{\textbf}

\title{Diagrams: Declarative Vector Graphics in Haskell}
\date{NY Haskell Users' Group \\ November 25, 2013}
\author{Brent Yorgey}
\titlegraphic{\includegraphics[width=1in]{Factorization.png}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{frame}[fragile]
  \titlepage
\end{frame}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% XXX remember to acknowledge others generously!

% \def\sectionimg{Factorization.png}

\section{Part I: Demo!}
\label{sec:demo}

\def\sectionimg{Factorization.png}

\section{Part II: Lessons for EDSL design}
\label{sec:lessons}

\begin{frame}{Take home}
  \begin{center}
    Domain analysis is hard! \bigskip

    \onslide<2-> Be in it for the long haul.
  \end{center}
\end{frame}

\section{History}
\label{sec:history}

%% XXX draw a timeline.

\begin{frame}
  \begin{center}
  April 2008. \bigskip

  Wanted: declarative, programmatic drawing.
  \end{center}
\end{frame}

\begin{frame}
  \begin{center}
  \begin{minipage}[c]{0.2\paperwidth}
    \includegraphics[width=1in]{inkscape-logo.png}
  \end{minipage}
  \begin{minipage}[c]{0.2\paperwidth}
  \includegraphics[width=1in]{gimp-logo.png}
  \end{minipage}
  \begin{minipage}[c]{0.2\paperwidth}
  \includegraphics[width=1in]{MPlogo.png}
  \end{minipage} \medskip
  \begin{minipage}[c]{0.2\paperwidth}
  \includegraphics[width=1in]{Asymptote-logo.png}
  \end{minipage}

  PGF/TikZ
  \end{center}
\end{frame}

\begin{frame}
  \begin{center}
    ``How hard could it be?'' \bigskip
  \end{center}
\end{frame}

\begin{frame}
  \begin{center}
    After two weeks of feverish hacking, diagrams was born! \bigskip

    \onslide<2>{It sucked.}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Data.Colour.Palette.ColorSet
      dia = circle 1 `appends`
            (zip (iterate (rotateBy (1/7)) unitX)
                 (zipWith fc (map d3Colors1 [0..9])
                   (map circle [0.2,0.5,1,0.3,0.6,0.4,0.1]))
                 )
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=300,height=200]
      import Data.Colour.Palette.ColorSet
      dia = decorateTrail (square 3) (zipWith named [0 :: Int ..] (repeat (square 1)))
          # lc black
          # connectOutside (2 :: Int) (0 :: Int)
          # lc blue
          # connectOutside (3 :: Int) (1 :: Int)
          # lc green
    \end{diagram}
  \end{center}
\end{frame}

% Went off and thought for a year.

\def\sectionimg{path.pdf}

\section{Paths}
\label{sec:paths}

\begin{frame}
  \begin{center}
    What is a \emph{path}?
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=300]
      import Diagrams

      dia = showTrail theTrail # centerXY # pad 1.1
    \end{diagram} \medskip

    \begin{spec}
      type Path = [Point]
    \end{spec}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Problem 1}
  % XXX if time: make different colors
  \begin{center}
    \begin{diagram}[width=300]
      import Diagrams

      p = showTrail theTrail

      dia = (p <> p # rotateBy (-1/13) # translate (3 ^& (-2)))
            # centerXY
         <> square 0.25 # rotateBy (1/8)
    \end{diagram}

    ?
  \end{center}
\end{frame}

\begin{frame}[fragile]
  % XXX if time: make different colors.  Show offset arrow.
  \begin{diagram}[width=300]
    import Diagrams

    o :: R2
    o = 3 ^& (-2)

    off :: R2
    off = trailOffset theTrail

    dia = showTrail (theTrail <> theTrail # rotateBy (-1/13))
       <> showTrail theTrail # rotateBy (-1/13) # translate o
          # dashing [0.1,0.1] 0
       <> arrowAt (origin .+^ o) (off ^-^ o)
          # dashing [0.05, 0.1] 0 # lc grey
  \end{diagram}
\end{frame}

\begin{frame}[fragile]{Problem 2}
  \begin{center}
    \begin{diagram}[width=300]
      import Diagrams
      dia = (showTrail $ cubicSpline False (trailVertices (theTrail `at` origin)))
          # centerXY # pad 1.1
    \end{diagram}
    %$
    \begin{spec}
      type Path = [(P2, CurveSpec)] ?
    \end{spec}
  \end{center}
\end{frame}

\section{Affine spaces}
\label{sec:affine}

\begin{frame}{Find the bug}
  \begin{spec}
    type Point   = (Double, Double)
    type Vector  = (Double, Double)

    instance (Num a, Num b) => Num (a, b) where
      ...

    parallelogram :: Point -> Point -> Point -> Point
    parallelogram p1 p2 p3 = p1 - p3 - p2
  \end{spec}
\end{frame}

\begin{frame}{Affine spaces for programmers}
  \begin{center}
    \emph{Confusing points and vectors is a type error!}
  \end{center}
\end{frame}

\begin{frame}[fragile]{Affine spaces}
  \begin{center}
    \begin{diagram}[width=200]
      d   = (dot # named 'A' <> dot # named 'B' # translate (10 ^& 7))
          # connectOutside 'A' 'B'
          # scale 0.2
          # centerXY # pad 1.1
      dot = circle 0.5 # fc black
      dia = d <> d # translate (1 ^& (-1))
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}
  \begin{center}
    \[ |translate (p1 - p2) == translate p1 - translate p2| \]
    \medskip

    \onslide<2>Translations apply to points but not to vectors!
  \end{center}
\end{frame}

\begin{frame}
  \begin{center}
  \begin{spec}
    (^+^)  :: Vector  -> Vector  -> Vector
    (.+^)  :: Point   -> Vector  -> Point
    (.-.)  :: Point   -> Point   -> Vector
  \end{spec}
  \end{center}
\end{frame}

\section{\dots Paths Again}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=300]
    import Diagrams
    dia = showTrailSegs theTrail
        # centerXY # pad 1.1
    \end{diagram}

    \begin{spec}
      type Path = [Vector]
    \end{spec}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=300]
      import Diagrams
      dia = (showTrailSegs $ cubicSpline False (trailVertices (theTrail `at` origin)))
          # centerXY # pad 1.1
    \end{diagram}
    %$
    \begin{spec}
      type Path = [Segment]
    \end{spec}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=200]
      import Diagrams
      dia = showTrail (cubicSpline True (trailVertices (theTrail `at` origin) ++ [2 ^& 0]))
          # centerXY # pad 1.1
    \end{diagram}

    \onslide<2>
    \begin{spec}
      type Path = ([Segment], Bool)?
    \end{spec}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=300]
      import Diagrams
      p = showTrail (cubicSpline True (trailVertices (theTrail `at` origin) ++ [2 ^& 0]))
      dia = (p <> p # rotateBy (-1/13) # translate (3 ^& (-2)))
            # centerXY
         <> square 0.25 # rotateBy (1/8)
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    $(
    \begin{minipage}[c]{0.5\textwidth}
      \begin{center}
    \begin{diagram}[width=150]
      import Diagrams
      dia = showTrailSegs theTrail # centerXY # pad 1.1
    \end{diagram}
      \end{center}
    \end{minipage}
    , True
    )$?
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    \begin{diagram}[width=200]
      import Diagrams
      dia =
        mconcat
        [ showTrailSegs theTrail
        , arrowBetween (origin .+^ trailOffset theTrail) origin
          # lc green # dashing [0.1,0.1] 0
        ]
        # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{frame}

\begin{frame}[fragile]
  \begin{center}
    $(
    \begin{minipage}[c]{0.5\textwidth}
      \begin{center}
    \begin{diagram}[width=150]
      import Diagrams
      dia = showTrailSegs (cubicSpline True (trailVertices (theTrail `at` origin) ++ [2 ^& 0]))
          # centerXY # pad 1.1
    \end{diagram}
      \end{center}
    \end{minipage}
    , True
    )$?
  \end{center}
\end{frame}

\begin{frame}{Our solution}
  \begin{spec}
    data Offset c v where
      OffsetOpen    :: Offset Open v
      OffsetClosed  :: v -> Offset Closed v

    data Segment c v  =  Linear (Offset c v)
                      |  Cubic v v (Offset c v)

  \end{spec}
\end{frame}

\begin{frame}
  \begin{spec}
    data Trail' l v where
      Line  :: [Segment Closed v]  -> Trail' Line v
      Loop  :: [Segment Closed v]  -> Segment Open v
                                   -> Trail' Loop v

    glueLine   :: Trail' Line v -> Trail' Loop v
    closeLine  :: Trail' Line v -> Trail' Loop v

    cutLine    :: Trail' Loop v -> Trail' Line v

    instance Monoid (Trail' Line v) where
      ...
  \end{spec}
\end{frame}

\begin{frame}[fragile]{Problem 3}
  \begin{center}
    \begin{diagram}[width=200,height=150]
      import Diagrams
      wibble = cubicSpline True (trailVertices (theTrail `at` origin) ++ [2 ^& 0])
      dia = (circle 3 <> pathFromLocTrail (wibble `at` ((-2) ^& (-1))))
          # stroke # fillRule EvenOdd # fc purple
          <> rect 7 1.5 # fc yellow
    \end{diagram}

    \onslide<2>
    \begin{spec}
      type Trail = [Segment] ...
      type Path  = [(Point, Trail)]
    \end{spec}
  \end{center}
\end{frame}

\begin{frame}{Our solution}
  \begin{spec}
    data Located a = Loc {loc :: Point (V a), unLoc :: a}

    newtype Path v = Path [Located (Trail v)]
  \end{spec}
\end{frame}

\end{document}