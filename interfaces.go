package z3s5

type Editing interface {
	StartInput(cb func())
	EndInput() (string, bool)
	EndInputCallback() func()
	Print(s string) (int, int)
	SetColor(bg bool, r, g, b, a uint8)
	Color(bg bool) (uint8, uint8, uint8, uint8)
}

type Audio interface {
	SystemSound(id int)
	SetVolume(fl float64)
}

type Runtime interface {
	EditorInterface() Editing
	SoundInterface() Audio
	Perm() Permissions
	SetPerm(Permissions) error
	RequestShutdown(errcode int)
	Enqueue(func())
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
