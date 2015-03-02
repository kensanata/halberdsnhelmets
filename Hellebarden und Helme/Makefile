GRAPHICS=$(wildcard graphics/*.jpg)
LATEX=pdflatex
MAKEINDEX=makeindex

all: Hellebarden-und-Helme.pdf

%.pdf: %.ltx ${GRAPHICS}
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<

force: Hellebarden-und-Helme.ltx
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<

once: Hellebarden-und-Helme.ltx
	${LATEX} $<
