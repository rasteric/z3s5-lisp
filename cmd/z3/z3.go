package main

import (
	"flag"
	"fmt"
	"os"

	z3 "github.com/rasteric/z3s5-lisp"
)

func main() {
	exec := flag.String("l", "", "load the specified file and execute it in a non-interactive session")
	flag.Parse()
	interp, err := z3.NewInterp(z3.NewBasicRuntime(z3.FullPermissions))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to start: %v\n", err)
		os.Exit(1)
	}
	interp.SetInteractive(*exec == "") // needs to be set before boot to prevent printing start banner
	err = interp.Boot()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Z3S5 Lisp failed to boot the standard prelude: %v\n", err)
		os.Exit(2)
	}
	interp.SafeEval(&z3.Cell{Car: z3.NewSym("protect-toplevel-symbols"), Cdr: z3.Nil}, z3.Nil)
	if *exec != "" {
		file, err := os.Open(*exec)
		if err != nil {
			panic(err)
		}
		defer file.Close()
		if !interp.Run(file) {
			os.Exit(1)
		}
		os.Exit(0)
	}
	interp.Run(nil)
}
