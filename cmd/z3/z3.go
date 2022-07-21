package main

import (
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"

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
	os.Stdout.WriteString("Welcome to Z3S5 Lisp on " + runtime.GOOS + "/" + runtime.GOARCH + " with " + strconv.Itoa(runtime.NumCPU()) + " cores!\nThe session started on " + time.Now().String() + ".\nUse (exit) to close the session.\n")
	interp.Run(nil)
}
