package z3s5

import (
	"bufio"
	"os"
	"os/signal"
	"syscall"

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
	os.Exit(errcode)
}

func (rt *BasicRuntime) Enqueue(f func()) {
	rt.tasks.Do(f)
}

var _ Runtime = &BasicRuntime{}

// BasicEditor provides only basic string entering for a REPL.
type BasicEditor struct {
	reader   *bufio.Reader
	writer   *bufio.Writer
	err      *bufio.Writer
	cb       func()
	s        string
	fgCol    string
	bgCol    string
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
	ed.writer.WriteString(s)
	ed.writer.Flush()
	return 1, 1
}

func (ed *BasicEditor) SetColor(bg bool, r, g, b, a uint8) {
	ed.colorSet = true
}

func (ed *BasicEditor) Color(bg bool) (uint8, uint8, uint8, uint8) {
	if bg {
		return 0, 0, 0, 255
	}
	return 255, 255, 255, 255
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
