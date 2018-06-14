GRAPHICS=$(wildcard graphics/*.jpg)
LATEX=pdflatex
MAKEINDEX=makeindex
FILES=Halberds-and-Helmets.pdf Halberds-and-Helmets-Ref-Guide.pdf

all: ${FILES}

%.pdf: %.ltx
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<
	mv $@ big-$@
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 \
		-dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET \
		-dBATCH -sOutputFile=$@ big-$@

shrink: Halberds-and-Helmets-Ref-Guide.pdf
	mv $@ big-$@
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 \
		-dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET \
		-dBATCH -sOutputFile=$@ big-$@

clean:
	rm -f \
	   *.an \
	   *.aux \
	   *.idx \
	   *.ilg \
	   *.ind \
	   *.log \
	   *.out \
	   big-*.pdf \
	   ${FILES}

test:
	perl test.pl
	# lacheck Halberds-and-Helmets.ltx
	# lacheck Halberds-and-Helmets-Ref-Guide.ltx
	# CHkTex -n24n1 Halberds-and-Helmets.ltx
	# ChkTex -n24n1 Halberds-and-Helmets-Ref-Guide.ltx

upload: $(FILES)
	scp -P 882 $^ alexschroeder.ch:alexschroeder.ch/pdfs/
