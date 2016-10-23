GRAPHICS=$(wildcard ../Hellebarden und Helme/graphics/*.jpg)
LATEX=pdflatex
MAKEINDEX=makeindex
FILES=Halberds-and-Helmets.pdf Halberds-and-Helmets-Ref-Guide.pdf

all: ${FILES}

%.pdf: %.ltx ${GRAPHICS}
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<

clean:
	rm *.an \
	   *.aux \
	   *.idx \
	   *.ilg \
	   *.ind \
	   *.log \
	   *.out \
	   ${FILES}
