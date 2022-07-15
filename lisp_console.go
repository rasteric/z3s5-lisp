package z3s5

import "fmt"

// Define_Console defines console-related commands, where the console is a OS terminal, not an editor console
// based on the virtual machine in PC.
func (interp *Interp) Define_Console() {

	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("console"), reflect})

	interp.Def("prin1", 1, func(a []any) any {
		fmt.Print(Str2(a[0], true))
		return a[0]
	})

	interp.Def("princ", 1, func(a []any) any {
		fmt.Print(Str2(a[0], false))
		return a[0]
	})

	interp.Def("terpri", 0, func(a []any) any {
		fmt.Println()
		return true
	})
}
