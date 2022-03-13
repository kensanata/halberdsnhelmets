JPGS=$(wildcard graphics-src/*.jpg)
PNGS=$(wildcard graphics-src/*.png)
SMALL=$(patsubst graphics-src/%.jpg,graphics-small/%.jpg,$(JPGS)) $(patsubst graphics-src/%.png,graphics-small/%.png,$(PNGS))
LATEX=pdflatex
MAKEINDEX=makeindex
FILES=Halberds-and-Helmets.pdf Halberds-and-Helmets-Ref-Guide.pdf

all: ${SMALL} ${FILES}

graphics-small/%.jpg: graphics-src/%.jpg
	convert -resize 300 "$<" "$@"

graphics-small/%.png: graphics-src/%.png
	convert -resize 300 "$<" "$@"

.PHONY: print online
print: ${JPGS} ${PNGS}
	rm -f graphics
	ln -sf graphics-src graphics
	rm ${FILES}
	make ${FILES}

online: ${SMALL}
	rm -f graphics
	ln -sf graphics-small graphics
	rm ${FILES}
	make ${FILES}

%.pdf: %.ltx
	${LATEX} $<
	${MAKEINDEX} `basename "$<" ".ltx"`.idx
	${LATEX} $<
	grep reference.*undefined $(basename $<).log || echo "References OK"

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
	rsync --archive --itemize-changes --rsh='ssh -p 882' $^ alexschroeder.ch:alexschroeder.ch/pdfs/

%.pdf: %.html %.css
	weasyprint $< $@

%.html: %-prefix %.html.tmp suffix
	cat $^ | sed 's/YYYY-MM-DD/$(shell date -I)/' > $@

%.html.tmp: %.md
	python3 -m markdown --file=$@ $<
