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

  % \AtBeginSection[]
  % {
  %   \begin{frame}<beamer>
  %     \frametitle{}

  %     \begin{center}
  %       % \includegraphics[width=2in]{\sectionimg}
  %       % \bigskip

  %       {\Huge \usebeamercolor[fg]{title}\insertsectionhead}
  %     \end{center}
  %   \end{frame}
  % }
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
\titlegraphic{\includegraphics[width=1in]{TreeBox}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
   \titlepage
\end{xframe}

\section{Introduction}
\label{sec:intro}

\begin{xframe}{What is diagrams?}
  \begin{itemize}
  \item<+-> Domain-specific language for vector graphics
  \item<+-> Embedded in Haskell
  \item<+-> 7+ years of development
  \item<+-> Active and creative community
  \end{itemize}
\end{xframe}

\begin{xframe}{Why an EDSL?}
  \begin{itemize}
  \item<+-> Powerful, programmable alternative to Illustrator,
    Inkscape, PGF/TikZ
  \item<+-> Enables artistic process.  Art influenced by tools.
  \end{itemize}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \url{http://projects.haskell.org/diagrams} \\
    (second Google result!)
    \bigskip

    \texttt{cabal install diagrams}
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

% \def\sectionimg{foo bar}   % XXX
\section{Demo: visualizing binary trees}
\label{sec:tree-variations}

\begin{xframe}{Tree visualizations}
  \begin{center}
    \begin{diagram}[width=200]
      import Diagrams

      dia :: Diagram B
      dia = treeVisuals (repeat (repeat False))
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge \usebeamercolor[fg]{title}Demo!}
  \end{center}
\end{xframe}

\section{More examples}
\label{sec:more-examples}



\end{document}