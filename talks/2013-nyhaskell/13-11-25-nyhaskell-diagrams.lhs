%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

\usepackage{haskell}
%include lhs2TeX-extra.fmt

\usepackage{brent}
\usepackage[backend=ps,extension=eps,outputdir=diagrams]{diagrams-latex}
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
        \includegraphics[width=1in]{\sectionimg}
        \bigskip

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

\title{Declarative vector graphics with diagrams}
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

\def\sectionimg{Factorization.png}

\section{Demo}
\label{sec:demo}

\def\sectionimg{Factorization.png}

\section{The birth of diagrams}
\label{sec:birth}

%% XXX draw a timeline.

\begin{frame}
  \begin{center}
  April 2008.

  Wanted: declarative, programmatic drawing.
  \end{center}
\end{frame}

\begin{frame}
  Inkscape + scripts;  Gimp + scripts; Metapost; Asymptote; TikZ
\end{frame}

\begin{frame}
  \begin{center}
    ``How hard could it be?''

    \onslide<2>(5 years later\dots)
  \end{center}
\end{frame}

\begin{frame}
  \begin{center}
    After two weeks of feverish hacking, diagrams was born!

    \onslide<2>{It sucked.}
  \end{center}
\end{frame}

\begin{frame}
  Used bounding boxes, had a terrible model for paths. Couldn't draw arrows.
\end{frame}

% Went off and thought for a year.

%% XXX add path image to go on section title page.

\section{Paths}
\label{sec:paths}

\begin{frame}
  What is a \emph{path}?
\end{frame}

\begin{frame}[fragile]
  \begin{diagram}[width=300]
    import Diagrams

    dia = showTrail (fromOffsets [1 ^& 1, 1 ^& (-0.5), 1 ^& 0.25, 0.25 ^& 1, 1 ^& 0])
  \end{diagram}
\end{frame}

\end{document}