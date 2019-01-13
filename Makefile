JPGS=$(wildcard graphics-src/*.jpg)
PNGS=$(wildcard graphics-src/*.png)
SMALL=$(patsubst graphics-src/%.jpg,graphics/%.jpg,$(JPGS)) $(patsubst graphics-src/%.png,graphics/%.png,$(PNGS))
LATEX=pdflatex
MAKEINDEX=makeindex
FILES=Halberds-and-Helmets.pdf Halberds-and-Helmets-Ref-Guide.pdf

all: ${SMALL} ${FILES}

graphics/%.jpg: graphics-src/%.jpg
	convert -resize 300 "$<" "$@"

graphics/%.png: graphics-src/%.png
	convert -resize 300 "$<" "$@"

%.pdf: %.ltx
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<
	grep reference.*undefined $(basename Halberds-and-Helmets).log || echo "References OK"

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
