all: lisp manuals

lisp: z3.go 
	go build -tags "fileio" z3.go

manuals: z3 distribution.lisp
	./z3 -l distribution.lisp && cd ../../docs/reference && make && cd ../../docs/usermanual && make
