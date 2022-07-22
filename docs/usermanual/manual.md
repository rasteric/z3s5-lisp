# Z3S5 Lisp User Manual

for version 2.3

## Introduction

Z3S5 Lisp can be used as a standalone interpreter or as an extension language embedded into Go programs. It is a traditional Lisp-1 dialect, where the suffix 1 means that symbols hold one value and this value might either represent functions and closures or data. This is in contrast to Lisp-2 language like CommonLisp in which symbols may hold functions in a separate slot. Scheme dialects are also Lisp-1 and Z3S5 Lisp shares many similarities with Scheme while also having features of traditional Lisp systems.

### Invoking Lisp

#### The Standalone Interpreter

In the directory `cmd/z3` there is an example standalone version `z3.go` that you can build on your system using `go build z3.go`. The interpreter is started in a terminal using `./z3`.

The `z3` interpreter is fairly limited. It starts a read-eval-reply loop until it is quit with the command `(exit [n])` where the optional number `n` is an integer for the Unix return code of the program. It reads one line of input from the command line and returns the result of evaluating it. Better editing capabilities and parenthesis matching are planned for the future; for now, the interpreter is more of a testing tool and a proof of concept and can be used as an example of how to implement your own Z3S5 standalone executable.

When an interpreter starts either in a standalone executable or when the `interp.Boot()` function is called in a Go program, then it first loads the standard prelude and help files in directory `embed`. These are embedded into the executable and so the directory is not needed to run the interpreter. After this start sequence, the interpreter checks whether there is a file named `init.lisp` in the executable directory---that is, the `z3` directory for standalone or the directory of the program that includes Z3S5 Lisp as a package. If there is such a file, then it is loaded and executed.

#### Using the Interpreter in Go

See `cmd/z3/z3.go` for an example of how to use Z3S5 Lisp in Go. It is best to include the package with `z3 "github.com/rasteric/z3s5-lisp"` so `z3` can be used as the package shortcut, since this has to be called often when writing extensions. The following snippet from `z3.go` imports the Lisp interpreter, boots the standard prelude, and runs an interactive read-eval-reply loop:

~~~
import (
	"fmt"
	"os"

	z3 "github.com/rasteric/z3s5-lisp"
)

func main() {
	interp, err := z3.NewInterp(z3.NewBasicRuntime(z3.FullPermissions))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to start: %v\n", err)
		os.Exit(1)
	}
	err = interp.Boot()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to boot the standard prelude: %v\n", err)
		os.Exit(2)
	}
	interp.SafeEval(&z3.Cell{Car: z3.NewSym("protect-toplevel-symbols"), Cdr: z3.Nil}, z3.Nil)
	interp.Run(nil)
}
~~~

What deserves explanation is the line starting with `interp.SafeEval`. This creates a list with the symbol `protect-toplevel-symbols` as car and an empty cdr and executes it within an empty environment. So it runs the equivalent of `(protect-toplevel-symbols)`, which is interpreted as a function call that applies `protect` to all toplevel symbols defined thus far. It is not put in the standard prelude because a common way to extend Z3S5 Lisp is to simply redefine some of its primitives. Once a symbol is protected, attempts of redefining or mutating it result in a security violation error. As long as the `Permissions` given to `NewBasicRuntime` have `AllowUnprotect` set to true, you may use `unprotect` to remove this safeguard again and redefine existing functions. Symbols are protected in the `z3` interpreter because it is easy to mess up the system with dynamic redefinitions. For example, you could redefine `(setq + -)` and `+` is suddenly interpreted as subtraction! However, redefining toplevel symbols is a very important feature. By using `unbind` on certain functions or setting them to something else like `(setq func (lambda () (error "not allowed to use func!")))` it is possible to create fine-grained access control to functions even when they are intrinsic or defined in the standard prelude. It is not possible to access an intrinsic function after it has been defined away in this manner. After you added all your toplevel symbols and removed or redefined the ones you do not like, you may thus protect all symbols and use `set-permissions` to revoke the privilege `AllowUnprotect`. This fixes the base vocabulary and any attempt to redefine it in the future will result in a security violation, since permissions can only be changed from less secure to more secure and never vice versa.

See the Chapter [Extending Z3S5 Lisp] for information about how to define your own functions in Z3S5 Lisp.

## Data Types

Basic data types of Z3S5 Lisp are:

- Bools: `t` and `nil`. The empty list `nil` is interpreted as false and any non-nil value is true; the symbol `t` is predefined as non-nil and usually used for true. 
- Symbols: `abracadabra`, `foo`, `bar`, `...hel%lo*_`. These have few restrictions, as the last example illustrates.
- Integers: `3`, `1`, `-28`. These are bignums with functions such as `+`, `-`, `sub`, `div`,...
- Floats: `3.14`, `1.92829` with a large range of functions with prefix `fl.`. Get a list with `(dump 'fl.)`
- UTF-8 Strings: `"This is a test."` with functions such as `str+`, `str-empty?`, `str-forall?`, `str-join`,... Get an overview with `(dump 'str)`.
- Lists: `'(a b c d e f)` with functions such as `car`, `cdr`, `mapcar`, `1st`, `2nd`, `member`, `filter`,...
- Arrays: `#(a b c d e f)` with functions such as `array-ref`, `array-set`, `array-len`, ...
- Dictionaries: `(dict)` with functions such as `get`, `set`, `dict->alist`, `dict->array`, `dict->values`, `dict-map`,...
- Boxed values with functions like `valid?`. Boxed values are used to embed foreign types like Go structures into the runtime system which cannot be automatically garbage collected. They typically require manual destruction to free memory. See the source code for `lisp_decimal.go` for an example of such an embedding.
- Futures: Futures encapsulate the result of a future computation and are returned by the special form `(future ...)` whose body evaluates to a future. The result of a future computation may be obtained with `(force <future>)`.
- Tasks: Tasks are heavyweight concurrency constructs similar to threads in other programming languages. See `(dump 'task)` for a list of functions.

Strings, lists, and arrays are also sequences testable with the `seq?` predicate. These support convenience successor functions like `map` and `foreach`. Check the `init.lisp` preamble in directory `embed/` for how these work.

Numbers are generally one bignum type for which `num?` is true and they will extend and shrink in their representation as necessary. However, numbers larger than 64 bit only support basic arithmetics and floating point arithmetics is limited to values that fit into a Float64. There is also a built-in decimal arithmetics extension with prefix `dec` for correct accounting and banking arithmetics and rounding---see `(dump 'dec.)` for a list of available decimal arithmetics functions and `lisp_decimal.go` for implementation details. 

## Writing Programs

### Finding Functions and Online Help

Built-in help on functions can be obtained with `(help func)` where the function name is not quoted. For example, `(help help)` returns the help entry for the `help` function. A list of bound toplevel symbols can be obtained with `(dump [sym])` where the optional symbol `sym` must be a quoted prefix. For example, `(dump)` returns all toplevel bindings and `(dump 'str)` returns a list of all bound symbols starting with "str". By convention, internal helper functions without help entry are prefixed with an underscore and omitted by `dump`. To get a list of all bindings, including those starting with an underscore, use `(dump-bindings)`.

### Peculiarities of Z3S5 Lisp

- Iterators over sequences use the order `(iterator sequence function)`, not sometimes one and sometimes the other. Exception: `memq` and `member` ask whether an element is a member of a list and have order `(member element list)`.

- There is no meaningful `eq`, equality is generally tested with `equal?`.

- All predicate names end in a question mark like in Scheme dialects. For example: `equal?`. There is no p-suffix like in other Lisps.

- `dict` data structures are multi-threading safe.

- There is support for futures and concurrent tasks.

### Basic Control Flow and a Few Examples

## Extending Z3S5 Lisp 

