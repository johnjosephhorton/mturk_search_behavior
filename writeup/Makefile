latexfile = mturk_search_behavior
figures = ./images/FIGB_coef_pooled.pdf
referencefile = search.bib

TEX = pdflatex -interaction=batchmode

../data/small_hits_grouped.csv:
	cd ../data && gunzip -d small_hits_grouped.csv.gz

$(figures): ../analysis/analysis.R ../data/small_hits_grouped.csv
	cd ../analysis && ./analysis.R 

all : $(latexfile).pdf clean
.PHONY: all

$(latexfile).pdf : $(figures) $(latexfile).tex $(referencefile)
	$(TEX) $(latexfile)
	$(TEX) $(latexfile)
	bibtex $(latexfile)
	$(TEX) $(latexfile)
	$(TEX) $(latexfile)
	$(TEX) $(latexfile)

clean: 
	rm *.out *.log *.blg *.bbl *.aux
