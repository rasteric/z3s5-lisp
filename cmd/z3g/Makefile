BUILT := $(shell git rev-parse --short HEAD)

all: fixversion lisp manuals

fixversion: ../../embed/version-template.lisp
	echo -n 'FIXING VERSION TAGS...' && sed 's,BUILT,"$(BUILT)",' ../../embed/version-template.lisp > ../../embed/version.lisp && echo 'DONE.'

lisp: z3g.go 
	echo -n 'COMPILING GUI EXECUTABLE...' && go build -tags "fileio db fts5" z3g.go && echo 'DONE.'

manuals: z3g distribution.lisp
	./z3g -l distribution.lisp && cd ../../docs/reference && echo -n 'CREATING REFERENCE MANUAL...' && make -s && echo 'DONE.' &&  cd ../../docs/usermanual && echo -n 'CREATING USER MANUAL...' && make -s && echo 'DONE.'

test: z3g test-distribution.lisp
	./z3g -l test-distribution.lisp
