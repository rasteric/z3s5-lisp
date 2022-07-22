all: docmake latexmake

docmake: manual.md
	pandoc -s -o build/manual.html manual.md --number-sections && pandoc -s -o build/manual.tex manual.md --number-sections --listings

latexmake: build/manual.tex
	 cd build && pdflatex manual.tex && pdflatex manual.tex &&  pdflatex manual.tex

clean: build/manual.html
	rm build/*

