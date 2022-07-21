package z3s5

import (
	"fmt"

	"github.com/gookit/color"
)

func (interp *Interp) Define_StyledText() {

	// (color 'selector) ==> (r g b a) get the current color, where selector in '(text back gfx).
	interp.Def("color", 1, func(a []any) any {
		var r, g, b uint8
		switch Str(a[0]) {
		case "text":
			r, g, b, _ = interp.pc.EditorInterface().Color(false)
		case "back":
			r, g, b, _ = interp.pc.EditorInterface().Color(true)
		default:
			panic(fmt.Sprintf(`unknown color selector '%s`, Str(a[0])))
		}
		c := color.RGBColor{r, g, b}
		return colorToList(c)
	})

	// (set-color selector color-list) set the r, g, b, a values of the color selected by selector,
	// where selector in '(text back gfx).
	interp.Def("set-color", 2, func(a []any) any {
		clist, ok := a[1].(*Cell)
		if !ok {
			panic(fmt.Sprintf("set-color: expected list of colors (red green blue alpha) as second argument, given %v",
				Str(a[1])))
		}
		carr := ListToArray(clist)
		colInts := interp.ExpectInts("set-color", carr, 0, 4)
		switch Str(a[0]) {
		case "text":
			interp.pc.EditorInterface().SetColor(false, uint8(colInts[0]), uint8(colInts[1]), uint8(colInts[2]),
				uint8(colInts[3]))
		case "back":
			interp.pc.EditorInterface().SetColor(true,
				uint8(colInts[0]), uint8(colInts[1]), uint8(colInts[2]),
				uint8(colInts[3]))
		default:
			panic(fmt.Sprintf(`unknown color selector '%s`, Str(a[0])))
		}
		return Void
	})

}
