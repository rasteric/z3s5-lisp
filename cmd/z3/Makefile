BUILT := $(shell git rev-parse --short HEAD)

all: fixversion lisp manuals

fixversion: ../../embed/version-template.lisp
	echo -n 'FIXING VERSION TAGS...' && sed 's,BUILT,"$(BUILT)",' ../../embed/version-template.lisp > ../../embed/version.lisp && echo 'DONE.'

lisp: z3.go 
	echo -n 'COMPILING EXECUTABLE...' && go build -tags "fileio db" z3.go && echo 'DONE.'

manuals: z3 distribution.lisp
	./z3 -l distribution.lisp && cd ../../docs/reference && echo -n 'CREATING REFERENCE MANUAL...' && make -s && echo 'DONE.' &&  cd ../../docs/usermanual && echo -n 'CREATING USER MANUAL...' && make -s && echo 'DONE.'
