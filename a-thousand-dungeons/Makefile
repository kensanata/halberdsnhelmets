all: a-thousand-dungeons.pdf

clean:
	rm -f *.html *.pdf

%.pdf: %.html %.css map1.svg map2.svg map3.svg
	weasyprint $< $@

%.html: %.md prefix suffix
	python3 -m markdown \
		--extension=markdown.extensions.tables \
		--extension markdown.extensions.smarty \
		--file=x$@ $<
	cat prefix x$@ suffix > $@
	rm x$@

