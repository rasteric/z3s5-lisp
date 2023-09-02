BUILT := $(shell git rev-parse --short HEAD)

all: fixversion lisp 

fixversion: ../../embed/version-template.lisp
	echo -n 'FIXING VERSION TAGS...' && sed 's,BUILT,"$(BUILT)",' ../../embed/version-template.lisp > ../../embed/version.lisp && echo 'DONE.'

lisp: z3g.go 
	echo -n 'COMPILING EXECUTABLE...' && go build -tags "fileio db fts5" z3g.go && echo 'DONE.'

test: z3g test-distribution.lisp
	./z3g -l test-distribution.lisp
