GRAPHICS=$(wildcard graphics/*.jpg)
LATEX=pdflatex
MAKEINDEX=makeindex

all: Hellebarden-und-Helme.pdf

%.pdf: %.ltx ${GRAPHICS}
	${LATEX} $<
	${MAKEINDEX} $<
	${LATEX} $<

force: Hellebarden-und-Helme.ltx
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`
	${LATEX} $<

once: Hellebarden-und-Helme.ltx
	${LATEX} $<

upload: Hellebarden-und-Helme.pdf
	scp $< epfarms.org:alexschroeder.ch/pdfs/
