# Z3S5 Lisp
an arcane Lisp dialect containing alien technology

[![GoDoc](https://godoc.org/github.com/rasteric/z3s5-lisp/go?status.svg)](https://godoc.org/github.com/rasteric/z3s5-lisp)
[![Go Report Card](https://goreportcard.com/badge/github.com/rasteric/z3s5-lisp)](https://goreportcard.com/report/github.com/rasteric/z3s5-lisp)

## Usage

Check out the basic lisp interpreter `z3.go` in folder `cmd/z3/` for an example of how to use Z3S5 Lisp in the Go language. You may also use the interpreter from the command line by running the `z3` binary. Notice that this implementation does not provide file or network access yet. This is planned as an option for the future. A user manual can be found in directory `docs/usermanual/`.

There is a build tag `nosound` you may use to leave out sound support. If this option is not specified, then the `beep` command produces a few basic sounds which are embedded into the executable and do slightly increase its size.

## Z3S5 Lisp Reference

Until more documentation is available, use `(dump [prefix])` in the Lisp system to get a list of toplevel bindings and use `(help symbol)` without quote to get information about a function, global symbol, or macro.

Z3S5 Lisp is an old-fashioned Lisp-1 dialect originally based on Nukata Lisp by SUZUKI Hisao. It uses real cons cells and is reasonably fast for an interpreter. (Depending on your mileage, it's definitely not optimized for speed.) There are some notable differences to other Lisps:

- Iterators over sequences use the order `(iterator sequence function)`, not sometimes one and sometimes the other. Exception: `memq` and `member` ask whether an element is a member of a list and have order `(member element list)`.

- There is no meaningful `eq`, equality is generally tested with `equal?`.

- Predicate names generally end in a question mark like in Scheme dialects except for `=` for numeric equality. Example: `equal?`. There is no p-suffix like in other Lisps.

- `dict` data structures are very powerful and multi-threading safe.

- There is experimental support for concurrency with green threads (aka goroutines).

Please don't complain it's not Common Lisp or R7RS Scheme! Z3S5 Lisp is deliberately a somewhat arcane, old-fashioned language mostly used for experimentation in a forthcoming retro-style [virtual Lisp machine](https://z3s5.com) and as an extension language for my own software projects.

## License

Permissive MIT License. Happy hacking! :smile_cat:


