all: The-Sewer-Prison.pdf

SVGS=map-1.svg map-2.svg map-3.svg map-4.svg map-5.svg map-6.svg
PNGS=$(subst .svg,.png,$(SVGS))

$(SVGS): map.svg
	./split-levels.pl $< map-%d.svg

%.png: %.svg
	inkscape $< --export-area-drawing --export-dpi=150 --export-background-opacity=255 --export-png=$@

%.pdf: %.html style.css
	weasyprint $< $@

%.html: %.html.tmp prefix suffix
	cat prefix $< suffix > $@

%.html.tmp: %.md
	python3 -m markdown \
		--extension=markdown.extensions.tables \
		--extension markdown.extensions.smarty \
		--file=$@ $<

The-Sewer-Prison.md: Intro.md Levels.md $(PNGS)
	date '+<p class="timestamp">%F</p>' > timestamp
	cat Intro.md timestamp Levels.md > $@

upload:
	rsync -avz The-Sewer-Prison.pdf sibirocobombus:alexschroeder.ch/pdfs/
