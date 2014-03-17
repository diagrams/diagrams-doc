%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

%include polycode.fmt

% \usepackage{brent}
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

\title{Declarative, Programmatic Vector Graphics in Haskell}
\date{Libre Graphics Meeting \\ 3 April, 2013}
\author{Brent Yorgey}
\titlegraphic{\includegraphics[width=1in]{Factorization.png}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{frame}[fragile]
  \titlepage
\end{frame}

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

%%% Since 2008.
%%% (Include chart of contribution volume across time for various
%%% different repos?)

\begin{frame}{History}

\end{frame}

%%% Domain-specific language for producing vector graphics. Intended
%%% to be higher-level than e.g. cairo but still general-purpose.
%%% Make it easy to build special-purpose visualizations on top of it.

%%% Declarative: high-level description of "what a drawing is" rather
%%% than "how to draw it".  (Motivation at least; somewhat arbitrary.)
%%% Show an example.

%%% Embedded in Haskell.  (Will have seen kuribas's talk?)  Say a few
%%% words about Haskell: high-level, functional, strongly typed, pure.
%%% Don't mean to start a language war, but this is a really fantastic
%%% real-world language for working at a high level.  Encourages
%%% thinking carefully about semantics and abstractions.

%%% Flexible: multiple vector spaces (2D, 3D); many backends (cairo,
%%% SVG, postscript, povray, ...)

%% Examples! (most with code)

%%% decide on some actual examples to include: each one should
%%% probably illustrate some particular feature

%%% things to mention/illustrate:
%%%% mathematical approach. semantics.
%%%% animation??
%%%% ???

%% LGM

%%% How does diagrams fit here?  Has things to learn and to offer.

%%% I would love to get feedback from people who actually know things
%%% about graphics!  I am enthusiastic and like communicating
%%% visually, but don't actually know a lot about the area.

%%% Domain-specific languages.  Capture inherent semantics of a
%%% domain.

%% Where we are going.

%%% Strong community.  Successful GSoC projects (two last year, one or
%%% more this year?)

%%% Editing operations
%%% GUI --- interactive, bidirectional
%%% Modelling animations & interactivity

\end{document}