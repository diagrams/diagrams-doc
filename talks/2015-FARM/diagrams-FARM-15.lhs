%% -*- mode: LaTeX; compile-command: "cabal --sandbox-config-file=/home/brent/src/diagrams/cabal.sandbox.config exec runhaskell Shake.hs" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

%include polycode.fmt

\usepackage{xspace}
\usepackage[backend=pgf,extension=pgf,input,outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\etc}{\textit{etc.}}
\newcommand{\eg}{\textit{e.g.}\xspace}
\newcommand{\ie}{\textit{i.e.}\xspace}

\newcommand{\theschool}{FARM}
\newcommand{\thelocation}{Vancouver, BC, Canada}
\newcommand{\thedate}{5 September 2015}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
        % \includegraphics[width=2in]{\sectionimg}
        % \bigskip

        {\Huge \usebeamercolor[fg]{title}\insertsectionhead}
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

\title{Diagrams}
\subtitle{A Functional EDSL for Vector Graphics}
\date{\theschool \\ \thelocation \\ \thedate}
\author{Ryan Yates \and Brent Yorgey}
\titlegraphic{\includegraphics[width=1in]{tree}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
   \titlepage
\end{xframe}

\begin{xframe}{What is diagrams?}
  \begin{itemize}
  \item<+-> Domain-specific language for vector graphics
  \item<+-> Embedded in Haskell
  \item<+-> 7+ years of development
  \item<+-> Large, active, creative community
  \end{itemize}
\end{xframe}

\begin{xframe}{Why an EDSL?}
  \begin{itemize}
  \item<+-> Powerful, programmable alternative to Illustrator,
    Inkscape, PGF/TikZ
  \item<+-> Tools influence the creative process
  \end{itemize}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \texttt{cabal install diagrams}
    \bigskip

    \url{http://projects.haskell.org/diagrams} \\
    (second Google result!)
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=\textwidth]{diagrams-website}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=\textwidth]{diagrams-gallery}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=\textwidth]{user-manual}
  \end{center}
\end{xframe}

\section{Demo: visualizing binary trees}
\label{sec:tree-variations}

\begin{xframe}{Tree visualizations}
  \begin{center}
    \begin{diagram}[width=200]
      import Diagrams

      dia :: IO (Diagram B)
      dia = treeVisuals (repeat (repeat False))
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[height=200]
      import Diagrams
      import TreeBox

      dia :: Diagram B
      dia = treeBoxes theTree
    \end{diagram}
  \end{center}
\end{xframe}

% \begin{xframe}{}
%   \begin{center}
%     \begin{diagram}[height=200]
%       import Diagrams
%       import BinLayout

%       dia :: Diagram B
%       dia = drawTree theTree
%     \end{diagram}
%   \end{center}
% \end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[height=200]
      import Diagrams
      import Dyck

      dia :: Diagram B
      dia = drawDyck False theTree
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
      {-# LANGUAGE FlexibleContexts #-}
      import Dyck
      import BinLayout
      import BoltzmannTrees
      import Diagrams (treeToTree)
      import qualified Diagrams.TwoD.Layout.Tree as LT

      Just theTree = seedGenM 6 11 0 genTree

      dia :: Diagram B
      dia = hsep 3 . map (sized (dims2D 10 10) . centerXY)
          $ [ bt, drawDyck False theTree ]

      bt = LT.renderTree (const mempty) (~~)
         . LT.symmLayout
         . treeToTree
         $ theTree
    \end{diagram}
  \end{center}
\end{xframe}

\section{More examples}
\label{sec:more-examples}

\begin{xframe}{3D Trees}
  \begin{center}
    \url{seeds/seeds.html}
      \bigskip

    \mbox{\url{http://www.cs.rochester.edu/u/ryates/art/seeds/}}
    \bigskip

    \includegraphics[width=1.5in]{tree}
  \end{center}
\end{xframe}

\begin{xframe}{Burrows-Wheeler Transform}
  \begin{center}
    \begin{diagram}[width=250]
      import Diagrams

      dia :: Diagram B
      dia = bwtDia
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{Weaving a Torus}
  \begin{center}
    \url{http://mathr.co.uk/blog/2013-04-05_weaving_a_torus.html}
    \bigskip

    \includegraphics[width=1.5in]{2013-04-05_weaving_a_torus_net_72dpi}
    \includegraphics[width=1.5in]{2013-04-05_weaving_a_torus}
  \end{center}
\end{xframe}

\begin{xframe}{Parking in Westminster}
  \begin{center}
    \url{https://idontgetoutmuch.wordpress.com/2013/10/23/parking-in-westminster-an-analysis-in-haskell/}
    \bigskip

    \includegraphics[width=3in]{parking}
  \end{center}
\end{xframe}

\begin{xframe}{ghc-events-analyze}
  \begin{center}
    \url{http://www.well-typed.com/blog/86/} \\
    \url{hackage.haskell.org/package/ghc-events-analyze}
    \bigskip

    \includegraphics[width=3.5in]{ghc-events-analyze}
  \end{center}
\end{xframe}

\begin{xframe}{Cretan maze}
  \begin{center}
    \url{http://www.corentindupont.info/blog/posts/2014-02-17-Cretan-Maze.html}
    \bigskip

    \includegraphics[width=2in]{maze}
  \end{center}
\end{xframe}

\begin{xframe}{Puzzles}
  \begin{center}
    \url{https://maybepuzzles.wordpress.com/2014/04/07/drawing-puzzles-with-the-haskell-diagrams-framework/}
    \bigskip

    \includegraphics[width=3in]{puzzle}
  \end{center}
\end{xframe}

\begin{xframe}{Stencil diagrams}
  \begin{center}
    \url{https://readerunner.wordpress.com/2014/04/29/red-black-neighbourhood-stencil-diagrams/}
    \bigskip

    \includegraphics[width=2in]{neighbourtemplates}
  \end{center}
\end{xframe}

\begin{xframe}{Num chart}
  \begin{center}
    \url{https://martingalemeasure.wordpress.com/2014/07/07/haskell-numeric-types-quick-reference/}
    \bigskip

    \includegraphics[width=2in]{Num-chart}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Large Happy diagramming!} \bigskip

    \includegraphics[width=1in]{tree}
  \end{center}
\end{xframe}

\end{document}