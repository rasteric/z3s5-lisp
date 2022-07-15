package z3s5

import (
	"bufio"
	"os"

	seq "github.com/jirenius/taskqueue"
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
	return &rt
}

func (rt *BasicRuntime) Editor() Editing {
	return rt.editor
}

func (rt *BasicRuntime) Sound() Audio {
	return rt.audio
}

func (rt *BasicRuntime) Perm() Permissions {
	return rt.perm
}

func (rt *BasicRuntime) RequestShutdown(errcode int) {
	os.Exit(errcode)
}

func (rt *BasicRuntime) Enqueue(f func()) {
	rt.tasks.Do(f)
}

var _ Runtime = &BasicRuntime{}

// BasicEditor provides only basic string entering for a REPL.
type BasicEditor struct {
	reader *bufio.Reader
	writer *bufio.Writer
	err    *bufio.Writer
	s      string
}

func NewBasicEditor() *BasicEditor {
	return &BasicEditor{
		reader: bufio.NewReader(os.Stdin),
		writer: bufio.NewWriter(os.Stdout),
		err:    bufio.NewWriter(os.Stderr),
	}
}

func (ed *BasicEditor) Print(s string) {
	ed.writer.WriteString(s)
}

func (ed *BasicEditor) StartInput() {
	ed.s, _ = ed.reader.ReadString('\n')
}

func (ed *BasicEditor) EndInput() (string, bool) {
	return ed.s, ed.s != ""
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

var _ Audio = &BasicAudio{}
