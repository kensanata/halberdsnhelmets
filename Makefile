JPGS=$(wildcard graphics-src/*.jpg)
PNGS=$(wildcard graphics-src/*.png)
SMALL=$(patsubst graphics-src/%.jpg,graphics-small/%.jpg,$(JPGS)) $(patsubst graphics-src/%.png,graphics-small/%.png,$(PNGS))
LATEX=pdflatex
MAKEINDEX=makeindex
FILES=Halberds-and-Helmets.pdf Halberds-and-Helmets-Ref-Guide.pdf

all: ${SMALL} ${FILES}

graphics/%.jpg: graphics-src/%.jpg
	convert -resize 300 "$<" "$@"

graphics/%.png: graphics-src/%.png
	convert -resize 300 "$<" "$@"

.PHONY: print online
print:
	rm graphics
	ln -sf graphics-src graphics
	rm ${FILES}
	make ${FILES}

online:
	rm graphics
	ln -sf graphics-small graphics
	rm ${FILES}
	make ${FILES}

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
	   ${FILES}

test:
	perl test.pl
	# lacheck Halberds-and-Helmets.ltx
	# lacheck Halberds-and-Helmets-Ref-Guide.ltx
	# CHkTex -n24n1 Halberds-and-Helmets.ltx
	# ChkTex -n24n1 Halberds-and-Helmets-Ref-Guide.ltx

upload: $(FILES)
	scp -P 882 $^ alexschroeder.ch:alexschroeder.ch/pdfs/
