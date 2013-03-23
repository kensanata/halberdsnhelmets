GRAPHICS=$(wildcard graphics/*.jpg)
LATEX=/usr/local/texlive/2012/bin/universal-darwin/pdflatex

all: Hellebarden-und-Helme.pdf

%.pdf: %.ltx ${GRAPHICS}
	${LATEX} $<
	${LATEX} $<

upload: Hellebarden-und-Helme.pdf
	scp $< epfarms.org:alexschroeder.ch/pdfs/
