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
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to start: %v", err)
		os.Exit(1)
	}
	err = interp.Boot()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to boot the standard prelude: %v", err)
		os.Exit(2)
	}
	fmt.Fprintf(os.Stdout, "Welcome to Z3S5 Lisp on "+runtime.GOOS+"/"+runtime.GOARCH+" with "+strconv.Itoa(runtime.NumCPU())+" cores. The session started on "+time.Now().String()+". Use (exit 0) to close the session when you're done.\n")
	interp.Run(nil)
}
