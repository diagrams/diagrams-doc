%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

%include lhs2TeX.fmt

% \usepackage{brent}
\usepackage[backend=cairo,extension=pdf,outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}
\usepackage{ulem}

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

\newenvironment{xframe}[1][]
  {\begin{frame}[fragile,environment=xframe,#1]}
  {\end{frame}}

% uncomment me to get 4 slides per page for printing
% \usepackage{pgfpages}
% \pgfpagesuselayout{4 on 1}[uspaper, border shrink=5mm]

% \setbeameroption{show only notes}

\renewcommand{\emph}{\textbf}

\title{Declarative, Programmatic Vector Graphics in Haskell}
\date{Libre Graphics Meeting \\ 3 April, 2013}
\author{Brent Yorgey}
\titlegraphic{\includegraphics[width=1in]{Factorization.png}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}
  \titlepage
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Diagrams (http://projects.haskell.org/diagrams) is a powerful
% domain-specific language, embedded in the Haskell programming
% language, for creating vector graphics and animations. I will show
% some examples of what is possible using the framework, particularly
% illustrating the benefits and power of Haskell as a substrate for a
% graphics language. I will talk briefly about the community
% surrounding its development, and explain some of the current
% features in development as well as our longer-term goals for the
% project.

% Brainstorming:
%   Start with very short "what is it".
%     - declarative, embedded in Haskell, many backends
%   Examples---visual + code.
%   Briefly mention community --- active contributors, successful GSoC projects
%   Features/approaches: very mathematical approach.  *Semantics*?
%     E.g. paths.  animations.
%   Where we are going:
%     + editing operations
%     + GUI --- interactive, bidirectional?
%     + animations & interactivity

%% Diagrams.
%% [All graphics in this talk were produced using diagrams!]

\begin{xframe}
  Diagrams %% TODO use logo?
  is a domain-specific language for producing vector graphics.

  %% TODO add picture with cairo, SVG, PS, POV-Ray etc. below it, and
  %% images representing higher-level stuff above it?

  %%% "Domain-specific language for producing vector
  %%% graphics. Intended to be higher-level than e.g. cairo but still
  %%% general-purpose. Make it easy to build special-purpose
  %%% visualizations on top of it."
\end{xframe}

\begin{xframe}{History}
  \begin{center}
    %% TODO (if time, e.g. on plane)
    %%% maybe try using Chart instead?!
    %%% add another graph showing total LOC (use Chart)
    %%% add colors for contributors!!
    %%% scale vertically to maximize slide space usage
    %%% add vertical lines with years marked
    \includegraphics[width=3in]{RepoRainbow}
  \end{center}
\end{xframe}

\begin{xframe}{Declarative}
%%% Declarative: high-level description of "what a drawing is" rather
%%% than "how to draw it".  (Motivation at least; somewhat arbitrary.)
%%% Show an example.

  \begin{center}
  Say \emph{what} to draw rather than \emph{how} to draw it. \vfill

  \begin{diagram}[width=100]
    dia = circle 1 # fc green |||||| square 2 # fc blue
  \end{diagram}
   \bigskip

  %% TODO: fix up Haskell typesetting
  \begin{spec}
    circle 1 # fc green ||| square 2 # fc blue
  \end{spec}

  \onslide<2>{\textit{Look ma, no coordinates!}}
  \end{center}
\end{xframe}

\begin{xframe}{Embedded}
%%% Embedded in Haskell.  (Will have seen kuribas's talk?)  Say a few
%%% words about Haskell: high-level, functional, strongly typed, pure.
%%% Don't mean to start a language war, but this is a really fantastic
%%% real-world language for working at a high level.  Encourages
%%% thinking carefully about semantics and abstractions.

  Embedded in Haskell.

  %% TODO: include Haskell logo.
\end{xframe}

\begin{xframe}{Flexible}
  %%% Flexible: multiple vector spaces (2D, 3D); many backends (cairo,
  %%% SVG, postscript, povray, ...)
  Flexible.
\end{xframe}

\begin{xframe}{Example: Basic geometry and arrows}
  \begin{center}
  \begin{diagram}[width=150]
    shapes = hcat' (with & sep .~ 3)
           [ square 2 # fc green # named "s"
           , circle 1 # fc blue  # named "c"
           ]
    dia = shapes # connectOutside' (with & gap .~ 0.2) "s" "c" # lw 0.03
        # frame 0.5
  \end{diagram}
  \begin{spec}
    shapes = hcat' (with & sep .~ 3)
           [ square 2  # fc green  # named "s"
           , circle 1  # fc blue   # named "c"
           ]
    dia = shapes
        # connectOutside' (with & gap .~ 0.2)
          "s" "c"
  \end{spec}
  \end{center}
\end{xframe}

\begin{xframe}{Example: Trees}
  %% XXX todo: pick better colors somehow.  Lowest leaves are too light.
  \begin{center}
  \begin{diagram}[width=200]
    import Diagrams.TwoD.Layout.Tree
    import Data.Maybe (fromJust)
    import Data.Colour.Palette.BrewerSet

    fibCalls :: Int -> BTree Int
    fibCalls 0 = leaf 0
    fibCalls 1 = leaf 1
    fibCalls n = BNode n (fibCalls (n-1)) (fibCalls (n-2))

    colors = brewerSet PuBuGn 9

    tree = renderTree'
             (\i -> circle 0.3 # lw 0 # fc (colors !! i))
             (\(i,p) (_,q) -> p ~~ q # lw 0.03 # lc (colors !! i))
         . fromJust . symmLayoutBin . fibCalls $ 8
    dia = tree # centerXY # frame 1
  \end{diagram}
  \begin{spec}
    fib 0 = leaf 0
    fib 1 = leaf 1
    fib n = BNode n (fib (n-1)) (fib (n-2))

    colors = brewerSet PuBuGn 9

    tree = renderTree'
      (\i -> circle 0.3 # lw 0 # fc (colors !! i))
      (\(i,p) (_,q) -> p ~~ q # lc (colors !! i))
      . fromJust . symmLayoutBin . fib $ 8
  \end{spec}
  \end{center}
\end{xframe}

\begin{xframe}{Example: Charts}
  Chart.
\end{xframe}

\begin{xframe}{Example: Parking in London}
  \begin{center}
    \includegraphics[width=3in]{parking}
  \end{center}
\end{xframe}
%% Examples! (most with code)

%%% decide on some actual examples to include: each one should
%%% probably illustrate some particular feature

%%% arrow from one thing to another.
%%% Fibonacci call tree.
%%% Sunflower
%%% unix poster?
%%% charts generated with Chart?
%%% parking diagram --- embedded.

%%% other features---animation

%%% things to mention/illustrate:
%%%% example showing why it's nice having it embedded.
%%%% mathematical approach. semantics.
%%%% animation??
%%%% ???

\begin{xframe}{Diagrams and LGM}
  \begin{diagram}[width=100]
    import Data.Colour.SRGB
    import Diagrams.Example.Logo

    lgm = iterate (/1.2) 1 # take 3
        # map (rotate (15 @@@@ deg) . alignBL . square)
        # hcat' (with & catMethod .~ Distrib
                      & sep .~ (1/1.2) / cos (15 * pi / 180) - 0.01)
        # lw 0 # fc (sRGB24 0 0 0x7e)
    -- 94 px, 15.36
    -- 78.x px, 15 deg
    -- 65 px, 15 deg
    -- 1.2 scaling factor.

    -- 82 px bewteen bottom corners.  Basically should be 
    -- middle square length / cos 15.

    -- color is 0x00007e.

    dia = hcat [logo, lgm # scale 10]
  \end{diagram}
\end{xframe}

%% LGM

%%% How does diagrams fit here?  Has things to learn and to offer.

%%% I would love to get feedback from people who actually know things
%%% about graphics!  I am enthusiastic and like communicating
%%% visually, but don't actually know a lot about the area.

%%% Domain-specific languages.  Capture inherent semantics of a
%%% domain.

\begin{xframe}{Community}
  %% Graph of unique nicks in IRC channel??  Nah, don't have enough
  %% data for that.
  Community.
\end{xframe}

%% Where we are going.

%%% Strong community.  Successful GSoC projects (two last year, one or
%%% more this year?)

\begin{xframe}{What's next?}
  %% Screenshot of Trello
  Trello.
\end{xframe}

\begin{xframe}{Editing}
  %% GSoC project
  %% Give some examples.
  Editing.
\end{xframe}

\begin{xframe}{Animations and interactivity}
  %% work with Andy and Nick.
  Animation.
\end{xframe}

\begin{xframe}{GUI}
  %% Bidirectional GUI.
  GUI.
\end{xframe}

\end{document}