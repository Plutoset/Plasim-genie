# $Id: Makefile 2118 2005-05-05 11:39:33Z cvs-msw $

# Makefile to build design paper

SRCS = land_manual.tex
DOCPS  = land_manual.ps
DOCPDF  = land_manual.pdf

# calling 'which' enables rule to fail if latex
# or dvips are not found.
all: $(SRCS)
	$(SHELL) -ec 'which latex; latex land_manual.tex'
	$(SHELL) -ec 'which latex; latex land_manual.tex'
	$(SHELL) -ec 'which dvips; dvips land_manual.dvi -o $(DOCPS)'
	$(SHELL) -ec 'which dvipdf; dvipdf land_manual.dvi $(DOCPDF)'

.PHONY: clean
clean:
	rm -f land_manual.dvi land_manual.toc land_manual.blg land_manual.log land_manual.bbl land_manual.aux $(DOCPS) $(DOCPDF)
