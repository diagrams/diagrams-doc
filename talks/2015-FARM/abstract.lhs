%% -*- mode: LaTeX; compile-command: "runhaskell Shake && open abstract.pdf" -*-

\documentclass[9pt,preprint,authoryear,nocopyrightspace]{sigplanconf}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lhs2TeX

%include polycode.fmt

% Use 'arrayhs' mode, so code blocks will not be split across page breaks.
\arrayhs

\renewcommand{\Conid}[1]{\mathsf{#1}}

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

\title{Diagrams---a functional EDSL for vector graphics}
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
approaches to graphics.

We propose to give a short \diagrams tutorial, highlighting
particularly the benefits of an embedded, functional DSL for the
representation and generation of vector graphics.  Although \diagrams
has made an implicit appearance at FARM before, in the context of a
work-in-progress presentation of a DSL for animation, it has never
been formally presented in and of itself.  \todo{Why diagrams is
  particularly appropriate for FARM.}

\section{Section}

\diagrams is

\todo{Explain why diagrams is interesting and/or what we will present
  in our tutorial, with some pretty pictures.}

\todo{Interesting ideas to highlight?: envelopes, DUAL Trees, pervasive use of
  monoids/monoid actions, affine spaces and related concepts
  (trails/paths)...}

\begin{figure}
\begin{center}
\begin{diagram}[width=150]
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import Data.Char (toLower)

t = nd [ nd [ nd $ leaves [B, B], lf B ]
       , nd [ nd [ lf H, nd $ leaves [A, A] ]
            , nd $ leaves [A, A]
            ]
       ]
  where nd     = Node Nothing
        lf x   = Node (Just x) []
        leaves = map lf

data Type = A || B || H  deriving Show

drawType x = mconcat
  [ text (map toLower (show x)) # italic # centerX
  , drawNode x ]
drawNode A = square 2 # fc yellow
drawNode B = circle 1 # fc red
drawNode H = circle 1 # fc white # dashingG [0.2,0.2] 0

renderT :: Tree (Maybe Type) -> Diagram B
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

t = nd [ nd [ nd $ leaves [B, B], lf B ]
       , nd [ nd [ lf H, nd $ leaves [A, A] ]
            , nd $ leaves [A, A]
            ]
       ]
  where nd     = Node Nothing
        lf x   = Node (Just x) []
        leaves = map lf

data Type = A | B | H  deriving Show

drawType x = mconcat
  [ text (map toLower (show x)) # italic # centerX
  , drawNode x ]
drawNode A = square 2 # fc yellow
drawNode B = circle 1 # fc red
drawNode H = circle 1 # fc white # dashingG [0.2,0.2] 0

renderT :: Tree (Maybe Type) -> Diagram B
renderT
  = renderTree (maybe mempty drawType) (~~)
  . symmLayout' (with & slHSep .~ 4 & slVSep .~ 3)

dia = renderT t # frame 0.5
\end{verbatim}
\end{center}
\caption{Tree with complete code} \label{fig:foobar}
\end{figure}

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
  \caption{Order-5 Hilbert curve with complete code}
  \label{fig:barfoo}
\end{figure}

\bibliographystyle{plainnat}
\bibliography{abstract}

\end{document}
