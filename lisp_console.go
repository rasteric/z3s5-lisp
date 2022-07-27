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

/*
  Copyright (c) 2019-2022 Erich H. Rast

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
