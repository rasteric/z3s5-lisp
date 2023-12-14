package z3s5

import "unicode"

func (interp *Interp) Define_Unicode() {

	// register this module
	reflect, ok := interp.GetGlobalVar(ReflectSym)
	if !ok {
		reflect = Nil
	}
	interp.SetGlobalVar(ReflectSym, &Cell{NewSym("unicode"), reflect})

	// (unicode.is-space? str) => bool
	interp.Def("unicode.is-space?", 1, func(a []any) any {
		return AsLispBool(unicode.IsSpace(GetAsRune("unicode.is-space?", a[0])))
	})

	// (unicode.is-punct? str) => bool
	interp.Def("unicode.is-punct?", 1, func(a []any) any {
		return AsLispBool(unicode.IsPunct(GetAsRune("unicode.is-punct?", a[0])))
	})

	// (unicode.is-symbol? str) => bool
	interp.Def("unicode.is-symbol?", 1, func(a []any) any {
		return AsLispBool(unicode.IsSymbol(GetAsRune("unicode.is-symbol?", a[0])))
	})

	// (unicode.is-title? str) => bool
	interp.Def("unicode.is-title?", 1, func(a []any) any {
		return AsLispBool(unicode.IsTitle(GetAsRune("unicode.is-title?", a[0])))
	})

	// (unicode.is-upper? str) => bool
	interp.Def("unicode.is-upper?", 1, func(a []any) any {
		return AsLispBool(unicode.IsUpper(GetAsRune("unicode.is-upper?", a[0])))
	})

	// (unicode.is-control? str) => bool
	interp.Def("unicode.is-control?", 1, func(a []any) any {
		return AsLispBool(unicode.IsControl(GetAsRune("unicode.is-control?", a[0])))
	})

	// (unicode.is-digit? str) => bool
	interp.Def("unicode.is-digit?", 1, func(a []any) any {
		return AsLispBool(unicode.IsDigit(GetAsRune("unicode.is-digit?", a[0])))
	})

	// (unicode.is-graphic? str) => bool
	interp.Def("unicode.is-graphic?", 1, func(a []any) any {
		return AsLispBool(unicode.IsGraphic(GetAsRune("unicode.is-graphic?", a[0])))
	})

	// (unicode.is-letter? str) => bool
	interp.Def("unicode.is-letter?", 1, func(a []any) any {
		return AsLispBool(unicode.IsLetter(GetAsRune("unicode.is-letter?", a[0])))
	})

	// (unicode.is-lower? str) => bool
	interp.Def("unicode.is-lower?", 1, func(a []any) any {
		return AsLispBool(unicode.IsLower(GetAsRune("unicode.is-lower?", a[0])))
	})

	// (unicode.is-mark? str) => bool
	interp.Def("unicode.is-mark?", 1, func(a []any) any {
		return AsLispBool(unicode.IsMark(GetAsRune("unicode.is-mark?", a[0])))
	})

	// (unicode.is-number? str) => bool
	interp.Def("unicode.is-number?", 1, func(a []any) any {
		return AsLispBool(unicode.IsNumber(GetAsRune("unicode.is-number?", a[0])))
	})

	// (unicode.is-print? str) => bool
	interp.Def("unicode.is-print?", 1, func(a []any) any {
		return AsLispBool(unicode.IsPrint(GetAsRune("unicode.is-print?", a[0])))
	})
}

// GetAsRune returns a rune given a string or goarith.Number representing a char
// and returns it, raises an error otherwise. The string may be any length but only
// the first rune is considered.
func GetAsRune(caller string, arg any) rune {
	switch arg.(type) {
	case string:
		return []rune(arg.(string))[0]
	default:
		n := ToInt(caller, arg)
		return rune(n)
	}
}
