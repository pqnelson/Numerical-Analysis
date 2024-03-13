TEX=pdflatex
BIB=bibtex
GFORT_FLAGS=-fdefault-real-8
FC=gfortran

all: doc

extract_tex:
	noweave -delay -index -latex numerical.nw > numerical.tex

with_bib:
	$(TEX) -draftmode numerical
	$(BIB) numerical
	makeindex numerical
	$(TEX) -draftmode -interaction=batchmode numerical
	$(TEX) -interaction=batchmode numerical

without_bib:
	$(TEX) -draftmode numerical
	makeindex numerical
	$(TEX) -interaction=batchmode numerical

doc: extract_tex with_bib

