VERSION := $(shell ../../cmd/z3/z3 -l ../../cmd/z3/version.lisp)

all: fixversion docmake

fixversion: manual0.md
	sed 's,Z3S5VERSION,"$(VERSION)",' manual0.md > manual.md

docmake: manual.md
	pandoc -s -o build/manual.html manual.md --number-sections && pandoc -s -o build/manual.pdf manual.md --from markdown --number-sections  --template eisvogel --listings 

clean: build/manual.html
	rm build/*

