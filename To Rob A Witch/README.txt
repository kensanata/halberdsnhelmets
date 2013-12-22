You need to fix the file URLs in the document. Make sure you use
"Wasili.jpg" instead of
"file:///Users/alex/Desktop/To%20Rob%20A%20Witch/Wasili.jpg".

"Save a Copy" as PDF.

cd "/Users/alex/Desktop/To Rob A Witch"
PATH=$PATH:/usr/local/texlive/2013/bin/universal-darwin
pdf90 To\ Rob\ A\ Witch.pdf
pdfjam --suffix shrunk --scale 0.95 "To Rob A Witch-rotated90.pdf"
pdfjam --outfile "To Rob A Witch by Alex Schroeder.pdf" "To Rob A Witch-rotated90-shrunk.pdf" "OGL.pdf"
rm "To Rob A Witch-rotated90.pdf"
rm "To Rob A Witch-rotated90-shrunk.pdf"
scp "To Rob A Witch by Alex Schroeder.pdf" psithyrus.epfarms.org:alexschroeder.ch/pdfs/
