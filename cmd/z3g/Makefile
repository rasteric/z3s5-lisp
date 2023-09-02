BUILT := $(shell git rev-parse --short HEAD)

all: fixversion lisp 

fixversion: ../../embed/version-template.lisp
	echo -n 'FIXING VERSION TAGS...' && sed 's,BUILT,"$(BUILT)",' ../../embed/version-template.lisp > ../../embed/version.lisp && echo 'DONE.'

lisp: z3u.go 
	echo -n 'COMPILING EXECUTABLE...' && go build -tags "fileio db fts5" z3u.go && echo 'DONE.'

test: z3u test-distribution.lisp
	./z3u -l test-distribution.lisp
