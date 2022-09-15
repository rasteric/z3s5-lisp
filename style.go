package z3s5

import (
	"fmt"

	"github.com/gookit/color"
)

func (interp *Interp) Define_StyledText() {

	// (color 'selector) ==> (r g b a) get the current color, where selector in '(text back gfx).
	interp.Def("color", 1, func(a []any) any {
		var c Color
		switch Str(a[0]) {
		case "text":
			c = interp.pc.EditorInterface().Color(false)
		case "back":
			c = interp.pc.EditorInterface().Color(true)
		default:
			panic(fmt.Sprintf(`unknown color selector '%s`, Str(a[0])))
		}
		col := color.RGBColor{c.R, c.G, c.B, c.A}
		return colorToList(col)
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
			interp.pc.EditorInterface().SetColor(false, Color{uint8(colInts[0]), uint8(colInts[1]), uint8(colInts[2]),
				uint8(colInts[3]), true})
		case "back":
			interp.pc.EditorInterface().SetColor(true,
				Color{uint8(colInts[0]), uint8(colInts[1]), uint8(colInts[2]),
					uint8(colInts[3]), true})
		default:
			panic(fmt.Sprintf(`unknown color selector '%s`, Str(a[0])))
		}
		return Void
	})

	// (reset-color) sets the default colors for both text and back (gfx color is not affected).
	interp.Def("reset-color", 0, func(a []any) any {
		interp.pc.EditorInterface().ResetColor()
		return Void
	})
}

/*
  Copyright (c) 2019-2022 Erich Rast

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
*/
