all: cmd/z3/z3.go
	cd cmd/z3 && make -s && make test

z3: cmd/z3/z3.go
	cd cmd/z3 && make lisp

test: z3
	cd cmd/z3 && make test
