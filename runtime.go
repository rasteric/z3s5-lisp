package z3s5

import (
	"bufio"
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"

	seq "github.com/jirenius/taskqueue"
	"github.com/rasteric/hooks"
)

// BasicRuntime is a minimal runtime system that only uses the console and basic sounds.
// It implements the Runtime interface.
type BasicRuntime struct {
	editor *BasicEditor
	audio  *BasicAudio
	perm   Permissions
	tasks  *seq.TaskQueue
}

func NewBasicRuntime(perm Permissions) *BasicRuntime {
	rt := BasicRuntime{}
	rt.editor = NewBasicEditor()
	rt.audio = NewBasicAudio()
	rt.perm = perm
	rt.tasks = seq.NewTaskQueue(8192)
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		rt.editor.Print("\n*** user interrupt, quitting ***\nBye-bye!\n")
		rt.RequestShutdown(1)
	}()
	return &rt
}

func (rt *BasicRuntime) EditorInterface() Editing {
	return rt.editor
}

func (rt *BasicRuntime) SoundInterface() Audio {
	return rt.audio
}

func (rt *BasicRuntime) Perm() Permissions {
	return rt.perm
}

func (rt *BasicRuntime) SetPerm(p Permissions) error {
	rt.perm = p
	return nil
}

func (rt *BasicRuntime) RequestShutdown(errcode int) {
	hooks.Exec(ShutdownHook, nil)
	os.Exit(errcode)
}

func (rt *BasicRuntime) Enqueue(f func()) {
	rt.tasks.Do(f)
}

var _ Runtime = &BasicRuntime{}

// SimpleColor is to simpify 8-bit color handling. It's an ad hoc structure.
type Color struct {
	R    uint8
	G    uint8
	B    uint8
	A    uint8
	Used bool
}

func (c Color) String() string {
	if !c.Used {
		return ""
	}
	return fmt.Sprintf("%v %v %v 255", c.R, c.G, c.B)
}

func (c Color) RGBA() (uint32, uint32, uint32, uint32) {
	return uint32(c.R), uint32(c.G), uint32(c.B), uint32(c.A)
}

var ColorWhite = Color{255, 255, 255, 255, true}
var ColorBlack = Color{0, 0, 0, 255, true}

// BasicEditor provides only basic string entering for a REPL.
type BasicEditor struct {
	reader   *bufio.Reader
	writer   *bufio.Writer
	err      *bufio.Writer
	cb       func()
	s        string
	fgCol    Color
	bgCol    Color
	colorSet bool
}

func NewBasicEditor() *BasicEditor {
	ed := &BasicEditor{
		reader: bufio.NewReader(os.Stdin),
		writer: bufio.NewWriter(os.Stdout),
		err:    bufio.NewWriter(os.Stderr),
	}

	return ed
}

func (ed *BasicEditor) Print(s string) (int, int) {
	lines := strings.Count(s, "\n")
	ed.writer.WriteString(s)
	ed.writer.Flush()
	return lines, lines
}

func (ed *BasicEditor) SetColor(bg bool, color Color) {
	ed.colorSet = true
	if bg {
		ed.bgCol = color
		return
	}
	ed.fgCol = color
}

func (ed *BasicEditor) Color(bg bool) Color {
	if bg {
		if ed.colorSet {
			return ed.bgCol
		}
		return ColorWhite
	}
	if ed.colorSet {
		return ed.fgCol
	}
	return ColorBlack
}

func (ed *BasicEditor) ResetColor() {

}

func (ed *BasicEditor) StartInput(cb func()) {
	b, _, err := ed.reader.ReadLine()
	if err != nil {
		return
	}
	ed.s = string(b)
	ed.cb()
}

func (ed *BasicEditor) EndInput() (string, bool) {
	return ed.s, ed.s != ""
}

func (ed *BasicEditor) EndInputCallback() func() {
	return ed.cb
}

var _ Editing = &BasicEditor{}

// BasicAudio plays a small number of predefined system sounds.
type BasicAudio struct {
}

func NewBasicAudio() *BasicAudio {
	return &BasicAudio{}
}

func (audio *BasicAudio) SystemSound(snd int) {
	playSystemSound(snd)
}

func (audio *BasicAudio) SetVolume(vol float64) {
	setVolume(vol)
}

var _ Audio = &BasicAudio{}

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
