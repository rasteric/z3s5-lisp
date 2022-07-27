VERSION := $(shell ../../cmd/z3/z3 -l ../../cmd/z3/version.lisp)

all: fixversion docmake 

fixversion: reference0.md
	sed 's,Z3S5VERSION,"$(VERSION)",' reference0.md > reference.md

docmake: reference.md
	pandoc -s -o build/reference.html reference.md --number-sections && pandoc -s -o build/reference.pdf reference.md --from markdown --number-sections  --template eisvogel --listings 

clean: build/reference.html
	rm build/*

