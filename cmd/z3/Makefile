BUILT := $(shell git rev-parse --short HEAD)

all: fixversion lisp

fixversion: ../../embed/version-template.lisp
	echo -n 'FIXING VERSION TAGS...' && sed 's,BUILT,"$(BUILT)",' ../../embed/version-template.lisp > ../../embed/version.lisp && echo 'DONE.'

lisp: z3.go 
	echo -n 'COMPILING EXECUTABLE...' && go build -tags "fts5 fileio db" z3.go && echo 'DONE.'

test: z3 test-distribution.lisp
	./z3 -l test-distribution.lisp
