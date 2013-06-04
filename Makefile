default: thesis

clean:
	rm -f *._tex
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg
	rm -f *.dvi
	rm -f *.pdf
	rm -f *.log
	rm -f *.ptb
	rm -f *.lof
	rm -f *.lol
	rm -f *.lot
	rm -f *.out
	rm -f *.synctex.gz
	rm -f text/*.log
	rm -f text/*.aux
	
thesis: clean
	latex -interaction=nonstopmode main.tex
	bibtex main
	pdflatex -interaction=nonstopmode main.tex
