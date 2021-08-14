TEX=pdflatex
BIB=bibtex
GFORT_FLAGS=-fdefault-real-8
FC=gfortran

all: doc

extract_tex:
	noweave -delay -index -latex numerical.nw > numerical.tex

with_bib:
	$(TEX) numerical
	$(BIB) numerical
	$(TEX) numerical
	$(TEX) numerical

without_bib:
	$(TEX) numerical
	$(TEX) numerical

doc: extract_tex with_bib

