package z3s5

type Editing interface {
	StartInput(cb func())          // called to start the input, callback is called when input ends
	EndInput() (string, bool)      // called when input ends
	EndInputCallback() func()      // the callback called when the input ends
	Print(s string) (int, int)     // print s in the current color, return number of lines scrolled and number of lines printed
	SetColor(bg bool, color Color) // set the current color
	Color(bg bool) Color           // get the current color
	ResetColor()                   // reset both foreground and background color to default
}

type Audio interface {
	SystemSound(id int)   // play a system sound with given ID
	SetVolume(fl float64) // set the volume of a sound played
}

type Runtime interface {
	EditorInterface() Editing    // return the editor interface
	SoundInterface() Audio       // return the audio interface
	Perm() Permissions           // return the current permissions of the interpreter
	SetPerm(Permissions) error   // set the current permissions, may return an error if not allowed
	RequestShutdown(errcode int) // request a shutdown of the interpreter - but it is not guaranteed
	Enqueue(func())              // enqueue an arbitrary function in a sequential queue - supposed to be GUI safe
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
