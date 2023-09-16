all: cmd/z3/z3.go cmd/z3g/z3g.go
	cd cmd/z3 && make -s && make test && cd ../z3g && make -s && make test

z3: cmd/z3/z3.go
	cd cmd/z3 && make lisp

z3g: cmd/z3g/z3g.go
	cd cmd/z3g && make lisp

test: z3 z3g
	cd cmd/z3 && make test && cd ../z3g && make test
