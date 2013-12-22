"Save a Copy" as PDF.

cd "$HOME/Source/halberdsnhelmets/Halberds and Helmets/Sepulchre of the Clone/"

PATH=$PATH:/usr/local/texlive/2013/bin/universal-darwin
pdfjam --outfile "Sepulchre of the Clone by Alex Schroeder.pdf" --a5paper "Sepulchre of the Clone.pdf" "OGL.pdf"
scp "Sepulchre of the Clone by Alex Schroeder.pdf" psithyrus.epfarms.org:alexschroeder.ch/pdfs/
