% bxart.cls
%  Layout reminiscent of B/X Dungeons & Dragons
% 2016-Jan-01 Peter H. Froehlich phf (at) acm.org
% This file is dedicated to the public domain.

\ProvidesClass{bxart}[2016/01/01 version 0.2 Layout reminiscent of B/X D&D]
\NeedsTeXFormat{LaTeX2e}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

\ProcessOptions \relax

% The B/X layout is in two columns and two-sided so we force the
% same here.
\LoadClass[twoside,twocolumn]{article}

% The fancyhdr package warns about \headheight being too small if
% we don't set it; I am doing this before geometry to ensure we
% still get a proper layout.
\setlength{\headheight}{14.5pt}

% Despite B/X being saddle-stitched (which should lie mostly flat
% so we don't need much in terms of inner margins) the B/X layout
% does have noticable inner margins. We do a little of that, not
% quite as much as B/X proper.
\RequirePackage[includehead,includefoot,inner=0.75in,outer=0.5in,top=0.5in,bottom=0.5in]{geometry}

% We'll go for two columns to keep the lines reasonably short, but
% we tweak the layout to be a little looser. Not quite as much as
% B/X however.
\setlength{\columnsep}{0.35in}

% Deal with widows, orphans, and keep LaTeX from stretching paragraphs
% apart.
\RequirePackage[all]{nowidow}
\raggedbottom

% The B/X layout does paragraphs like this (space between, no indent)
% so we'll do the same for now.
\RequirePackage{parskip}

% Just a hack to get \thetitle below. Otherwise we have to mess with
% LaTeX internals.
\RequirePackage{titling}

% Set up headers. Note that unlike B/X we don't capitalize everything
% because ugly. If you really want it differently, remove the patch
% below.
\RequirePackage{fancyhdr}

\fancypagestyle{plain}{%
  \fancyhead{}
  \fancyhead[LO,RE]{\bfseries\slshape \thetitle}
  \fancyhead[RO,LE]{\bfseries\slshape \nouppercase{\leftmark}}
  \fancyfoot{}
  \fancyfoot[C]{\thepage}%
  \renewcommand{\headrulewidth}{0pt}%
  \renewcommand{\footrulewidth}{0pt}%
}
\pagestyle{plain}

% No section numbers, no uppercase.
\renewcommand{\sectionmark}[1]{\markboth{#1}{}}
