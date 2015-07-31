GRAPHICS=$(wildcard ../Hellebarden und Helme/graphics/*.jpg)
LATEX=pdflatex
MAKEINDEX=makeindex

all: Halberds-and-Helmets.pdf

%.pdf: %.ltx ${GRAPHICS}
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<

force: Halberds-and-Helmets.ltx
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<

once: Halberds-and-Helmets.ltx
	${LATEX} $<

clean:
	rm Halberds-and-Helmets.aux \
	    Halberds-and-Helmets.idx \
	    Halberds-and-Helmets.ilg \
	    Halberds-and-Helmets.ind \
	    Halberds-and-Helmets.log \
	    Halberds-and-Helmets.out \
	    Halberds-and-Helmets.pdf
