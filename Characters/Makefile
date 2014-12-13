%.pdf: %.svg
	inkscape --file=$< --export-area-page --export-pdf=$@

test:
	prove t

translations:
	perl halberdsnhelmets.pl '/translation?'
