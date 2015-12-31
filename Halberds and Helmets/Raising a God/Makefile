%.pdf: %.svg
	inkscape "$<" --export-pdf="$@"

# 2000px wide should be good enough for printing on A4?

%.png: %.svg
	convert -resize 2000x "$<" "$@"
