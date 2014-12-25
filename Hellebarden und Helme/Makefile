GRAPHICS=$(wildcard graphics/*.jpg)
PANDOC=pandoc
CONVERT=ebook-convert
# CONVERT=/Applications/calibre.app/Contents/MacOS/ebook-convert
VIEW=ebook-viewer
# VIEW=/Applications/calibre.app/Contents/MacOS/ebook-viewer
LATEX=pdflatex
MAKEINDEX=makeindex
EMACS=emacs
# EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

all: Hellebarden-und-Helme.mobi

preview: Hellebarden-und-Helme.mobi
	${VIEW} $<

clean:
	rm -f Hellebarden-und-Helme.mobi
	rm -f Hellebarden-und-Helme.html
	rm -f theindex.html
	rm -f .*.orgx
	rm -f *.aux *.log *.out

%.mobi: %.html
	${CONVERT} $< $@ \
	--chapter=//h:h1 \
	--chapter-mark=pagebreak \
	--level1-toc=//h:h1 \
	--level2-toc=//h:h2 \
	--authors="Alex Schroeder" \
	--language=de \
	--cover=graphics/cover.jpg \
	--output-profile=kindle \
	--smarten-punctuation

.PRECIOUS: %.html

%.html: %.org
	${EMACS} -q -Q $< --batch -l project.el --kill

%.pdf: %.org
	pandoc $< -o $@

# %.pdf: %.ltx ${GRAPHICS}
# 	${LATEX} $<
# 	${MAKEINDEX} `basename "$<" ".ltx"`.idx
# 	${LATEX} $<

# force: Hellebarden-und-Helme.ltx
# 	${LATEX} $<
# 	${MAKEINDEX} `basename "$<" ".ltx"`.idx
# 	${LATEX} $<

# once: Hellebarden-und-Helme.ltx
# 	${LATEX} $<
