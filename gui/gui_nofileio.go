//go:build !fileio

package ui

import (
	"fmt"

	z3 "github.com/rasteric/z3s5-lisp"
)

// DefGUI defines the user interface functions. If you want to avoid polluting the namespace, use
// a config with a custom Prefix. Use DefaultConfig for a maximally permissive default configuration.
// Various security-sensitive settings such as allowing or disallowing creation of new windows can be adjusted
// in the Config. If you set these, be sure to also restrict the language using Z3S5 Lisp standard security tools,
// such as unbinding certain functions and then protecting them and disallowing unprotecting them again.
func DefGUI(interp *z3.Interp, config Config) {
	defGUINoFileIO(interp, config)
	if err := DefGUIAdditions(interp); err != nil {
		panic(fmt.Sprintf("Z3S5 Lisp internal error, failed to boot embedded GUI additions: %v", err))
	}
}
