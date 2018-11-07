\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hw-scrartcl}[2018/1/22 Homework class]

\RequirePackage{kvoptions}

\SetupKeyvalOptions{family=hw, prefix=hw@}

\DeclareStringOption{title}
\DeclareStringOption{subtitle}
\DeclareStringOption[Damian Pavlyshyn]{author}

% Additional options passed to scrartcl
\DeclareDefaultOption{\PassOptionsToClass{\CurrentOption}{scrartcl}}

\ProcessKeyvalOptions*

\LoadClass{scrartcl}

% Set up title
\title{\hw@title}
\subtitle{\hw@subtitle}
\author{\hw@author}

% Set header with subtitle and author
\DeclareVoidOption{header}{
  \RequirePackage[headsepline]{scrlayer-scrpage}
  \pagestyle{scrheadings}
  \lohead{\hw@subtitle}
  \rohead{\hw@author}
}

\ProcessKeyvalOptions*

% Set KOMA fonts to standard LaTeX fonts
\setkomafont{disposition}{\normalfont\bfseries} % Body font
\setkomafont{pagehead}{\normalfont\normalcolor} % Header fond