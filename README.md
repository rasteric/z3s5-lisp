# Z3S5 Lisp
an arcane Lisp dialect containing alien technology

## Usage

Check out the basic lisp interpreter `z3.go` in folder `cmd/z3/` for an example of how to use Z3S5 Lisp in the Go language. You may also use the interpreter from the command line by running the `z3` binary. Notice that this implementation does not provide file or network access. I'm planning to add this later as an option. There is a build tag `nosound` you may use to leave out sound support. Otherwise, a dependence on alsa on Linux is introduced and a few system sound files for the `beep' command are integrated into the executable file using the `z3s5-lisp` package.

## Z3S5 Lisp Reference

Although a larger virtual machine using Z3S5 Lisp has comprehensive online help, this documentation has not yet been ported to this package. For now, you'll have to check the source code in the files starting with `lisp` in their name.

Z3S5 Lisp is a very old-fashioned Lisp-1 dialect. It uses real cons cells and is reasonably fast, though not optimized for speed. It is based on Nukata Lisp by SUZUKI Hisao. However, there are some larger changes that make it distinct from most Lisps you might have seen:

- Iterators over sequences use the order `(iterator sequence function)`, not sometimes one and sometimes the other, with the exception of the `member` functions.

- There is no meaningful `eq` and equality is generally tested with `equal?`.

- All predicate names end in a question mark like in Scheme dialects. No `p`-suffix like in other Lisps.

- `dict` data structures are very powerful but not super-fast; they are multi-threading safe.

Please don't complain it's not Common Lisp or R7RS Scheme! Z3S5 is deliberately a somewhat arcane, old-fashioned language mostly used for experimentation in a forthcoming retro-style [virtual Lisp machine](https://z3s5.com) and as an extension language for my own software projects.  
