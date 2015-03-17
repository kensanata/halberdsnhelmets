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

clean:
	rm Hellebarden-und-Helme.aux \
	    Hellebarden-und-Helme.idx \
	    Hellebarden-und-Helme.ilg \
	    Hellebarden-und-Helme.ind \
	    Hellebarden-und-Helme.log \
	    Hellebarden-und-Helme.out \
	    Hellebarden-und-Helme.pdf
