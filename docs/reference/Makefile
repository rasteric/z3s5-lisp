all: docmake latexmake

docmake: reference.md
	pandoc -s -o build/reference.html reference.md --number-sections && pandoc -s -o build/reference.tex reference.md --number-sections --listings

latexmake: build/reference.tex
	 cd build && pdflatex reference.tex && pdflatex reference.tex &&  pdflatex reference.tex

clean: build/reference.html
	rm build/*

