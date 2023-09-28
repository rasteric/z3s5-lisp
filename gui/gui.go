package ui

import (
	"bytes"
	_ "embed"
	"errors"
	"fmt"
	"image/color"
	"math"
	"net/url"
	"sync"
	"sync/atomic"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/data/validation"
	"fyne.io/fyne/v2/driver/desktop"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
	"github.com/nukata/goarith"
	z3 "github.com/rasteric/z3s5-lisp"
)

//go:embed embed/gui-help.lisp
var helpGUIFile []byte

var counter uint64
var storage sync.Map
var revstore sync.Map
var apl fyne.App
var mainWin fyne.Window

var VersionSym = z3.NewSym("*z3s5-version*")
var GUISym = z3.NewSym("gui")
var FyneSym = z3.NewSym("fyne2")
var IsQuitSym = z3.NewSym("is-quit")
var IsSeparatorSym = z3.NewSym("is-separator")
var DisabledSym = z3.NewSym("disabled")
var CheckedSym = z3.NewSym("checked")
var WrapOffSym = z3.NewSym("none")
var WrapBreakSym = z3.NewSym("break")
var WrapWordSym = z3.NewSym("word")

// key name symbols, for a more Lispy interface to logical key names
var KeyEscape = z3.NewSym("escape")
var KeyReturn = z3.NewSym("return")
var KeyTab = z3.NewSym("tab")
var KeyBackspace = z3.NewSym("backspace")
var KeyInsert = z3.NewSym("insert")
var KeyDelete = z3.NewSym("delete")
var KeyRight = z3.NewSym("right")
var KeyLeft = z3.NewSym("left")
var KeyDown = z3.NewSym("down")
var KeyUp = z3.NewSym("up")
var KeyPageUp = z3.NewSym("page-up")
var KeyPageDown = z3.NewSym("page-down")
var KeyHome = z3.NewSym("home")
var KeyEnd = z3.NewSym("end")
var KeyF1 = z3.NewSym("f1")
var KeyF2 = z3.NewSym("f2")
var KeyF3 = z3.NewSym("f3")
var KeyF4 = z3.NewSym("f4")
var KeyF5 = z3.NewSym("f5")
var KeyF6 = z3.NewSym("f6")
var KeyF7 = z3.NewSym("f7")
var KeyF8 = z3.NewSym("f8")
var KeyF9 = z3.NewSym("f9")
var KeyF10 = z3.NewSym("f10")
var KeyF11 = z3.NewSym("f11")
var KeyF12 = z3.NewSym("f12")
var KeyEnter = z3.NewSym("enter")
var Key0 = z3.NewSym("key0")
var Key1 = z3.NewSym("key1")
var Key2 = z3.NewSym("key2")
var Key3 = z3.NewSym("key3")
var Key4 = z3.NewSym("key4")
var Key5 = z3.NewSym("key5")
var Key6 = z3.NewSym("key6")
var Key7 = z3.NewSym("key7")
var Key8 = z3.NewSym("key8")
var Key9 = z3.NewSym("key9")
var KeyA = z3.NewSym("a")
var KeyB = z3.NewSym("b")
var KeyC = z3.NewSym("c")
var KeyD = z3.NewSym("d")
var KeyE = z3.NewSym("e")
var KeyF = z3.NewSym("f")
var KeyG = z3.NewSym("g")
var KeyH = z3.NewSym("h")
var KeyI = z3.NewSym("i")
var KeyJ = z3.NewSym("j")
var KeyK = z3.NewSym("k")
var KeyL = z3.NewSym("l")
var KeyM = z3.NewSym("m")
var KeyN = z3.NewSym("n")
var KeyO = z3.NewSym("o")
var KeyP = z3.NewSym("p")
var KeyQ = z3.NewSym("q")
var KeyR = z3.NewSym("r")
var KeyS = z3.NewSym("s")
var KeyT = z3.NewSym("t")
var KeyU = z3.NewSym("u")
var KeyV = z3.NewSym("v")
var KeyW = z3.NewSym("w")
var KeyX = z3.NewSym("x")
var KeyY = z3.NewSym("y")
var KeyZ = z3.NewSym("z")
var KeySpace = z3.NewSym("space")
var KeyTick = z3.NewSym("tick")
var KeyComma = z3.NewSym("comma")
var KeyMinus = z3.NewSym("minus")
var KeyPeriod = z3.NewSym("dot")
var KeySlash = z3.NewSym("slash")
var KeyBackslash = z3.NewSym("backslash")
var KeyLeftBracket = z3.NewSym("left-bracket")
var KeyRightBracket = z3.NewSym("right-bracket")
var KeySemicolon = z3.NewSym("semicolon")
var KeyEqual = z3.NewSym("equal")
var KeyAsterisk = z3.NewSym("asterisk")
var KeyPlus = z3.NewSym("plus")
var KeyBackTick = z3.NewSym("back-tick")
var KeyShift = z3.NewSym("shift")
var KeyControl = z3.NewSym("control")
var KeyAlt = z3.NewSym("alt")
var KeySuper = z3.NewSym("super")
var KeyUnknown = z3.NewSym("unknown")

// used to work around Go prohibition to hash functions
type validatorWrapper struct {
	fn fyne.StringValidator
}

// used for transforming hyperlink URLs
type HyperlinkCheckFunc = func(url *url.URL) *url.URL

// Config stores the configuration for the GUI, which is global and cannot be changed once set.
// This is passed to DefUI. Use DefaultConfig for a maximally permissive default configuration.
type Config struct {
	HyperlinksAllowed   bool               // if true, hyperlinks can be created
	CheckHyperlinks     HyperlinkCheckFunc // if set, transform hyperlink URLs during creation
	WindowsAllowed      bool               // is true, windows can be created
	Prefix              string             // a prefix for all identifiers defined (excluding ".")
	ClipboardGetAllowed bool               // allow getting the clipboard content (could be used to monitor for passwords)
	ClipboardSetAllowed bool               // allow setting the clipboard content
}

var DefaultConfig = Config{
	HyperlinksAllowed:   true,
	CheckHyperlinks:     HyperlinkCheckFunc(func(url *url.URL) *url.URL { return url }),
	WindowsAllowed:      true,
	Prefix:              "",
	ClipboardGetAllowed: true,
	ClipboardSetAllowed: true,
}

var RestrictedConfig = Config{
	HyperlinksAllowed:   false,
	CheckHyperlinks:     HyperlinkCheckFunc(func(url *url.URL) *url.URL { panic("hyperlinks are not allowed by security policy!") }),
	WindowsAllowed:      false,
	Prefix:              "",
	ClipboardGetAllowed: false,
	ClipboardSetAllowed: false,
}

var cfg Config

// put stores an object and returns a number in Z3S5 Lisp format.
func put(obj any) any {
	n := atomic.AddUint64(&counter, 1)
	revstore.Store(obj, n)
	storage.Store(n, obj)
	return goarith.AsNumber(int64(n))
}

// get retrieves an object by its number and returns the object and true if found, nil and false otherwise.
func get(n any) (any, bool) {
	k := z3.ToInt64("user interface", n)
	return storage.Load(uint64(k))
}

// getID retrieves an object's ID if it is stored, returns false otherwise.
func getID(obj any) (any, bool) {
	n, ok := revstore.Load(obj)
	if !ok {
		return goarith.AsNumber(-1), false
	}
	return goarith.AsNumber(int64(n.(uint64))), true
}

// mustGet attempts to get the object by number stored in array a at index n, panics otherwise.
func mustGet(caller, arg string, a []any, n int) any {
	obj, ok := get(a[n])
	if !ok {
		panic(fmt.Sprintf("%v: expected valid %v ID as argument %v, given %v", caller, arg, n+1, z3.Str(a[n])))
	}
	return obj
}

// mustGet1 gets the object given in the interface as number, panics otherwise.
func mustGet1(caller, arg string, a any) any {
	obj, ok := get(a)
	if !ok {
		panic(fmt.Sprintf("%v: expected valid %v ID as argument, given %v", caller, arg, z3.Str(a)))
	}
	return obj
}

// clear delete any object association for the given numeric key.
func clear(n any) {
	k, ok := n.(goarith.Int64)
	if ok {
		obj, loaded := storage.LoadAndDelete(uint64(k))
		if loaded {
			revstore.Delete(obj)
		}
	}
}

// RunGUI initializes the user interface and starts running it. The function blocks until the application
// is quit, e.g. via ShutDownUI. Since it blocks, the Lisp interpreter must be started in parallel.
func RunGUI() {
	apl = app.New()
	mainWin = apl.NewWindow("")
	mainWin.SetMaster()
	apl.Run()
}

// ShutdownGUI shuts down the user interface. Any attempt to use it afterwards may result in a panic. This should
// be called before closing the application, e.g. in a defer statement.
func ShutDownGUI() {
	storage.Range(func(key interface{}, value interface{}) bool {
		storage.Delete(key)
		return true
	})
	mainWin.Close()
	apl.Quit()
}

// CloseGUI closes all existing windows and attempts to free existing resources but does not quit the
// the internal main application, so new windows can be opened again.
func CloseGUI() {
	storage.Range(func(key interface{}, value interface{}) bool {
		if win, ok := value.(fyne.Window); ok {
			win.Hide()
		}
		return true
	})
}

// DefGUIHelp defines the help definitions for the GUI functions.
func DefGUIHelp(interp *z3.Interp) error {
	help := bytes.NewReader(helpGUIFile)
	if !interp.Run(help, z3.NewInternalSource("embed/gui-help.lisp", "")) {
		return errors.New(`Z3S5 Lisp GUI help definitions failed`)
	}
	return nil
}

// DefGUI defines the user interface functions. If you want to avoid polluting the namespace, use
// a config with a custom Prefix. Use DefaultConfig for a maximally permissive default configuration.
// Various security-sensitive settings such as allowing or disallowing creation of new windows can be adjusted
// in the Config. If you set these, be sure to also restrict the language using Z3S5 Lisp standard security tools,
// such as unbinding certain functions and then protecting them and disallowing unprotecting them again.
func DefGUI(interp *z3.Interp, config Config) {

	// register this module
	reflect, ok := interp.GetGlobalVar(z3.ReflectSym)
	if !ok {
		reflect = z3.Nil
	}
	interp.SetGlobalVar(z3.ReflectSym, &z3.Cell{Car: GUISym, Cdr: &z3.Cell{Car: FyneSym, Cdr: reflect}})

	version, ok := interp.GetGlobalVar(VersionSym)
	if ok {
		interp.SetGlobalVar(VersionSym, version.(string)+"-"+GUISym.String()+"."+FyneSym.String())
	}

	cfg := config

	pre := func(s string) string {
		if cfg.Prefix == "" {
			return s
		}
		return cfg.Prefix + "." + s
	}
	// WINDOW

	// (new-window <title-string>)
	interp.Def(pre("new-window"), 1, func(a []any) any {
		if !cfg.WindowsAllowed {
			panic(pre("new-window: creating new windows is not permitted!"))
		}
		return put(apl.NewWindow(a[0].(string)))
	})

	// (set-window-content <window> <canvas-object>)
	interp.Def(pre("set-window-content"), 2, func(a []any) any {
		win, ok := get(a[0])
		if !ok {
			panic(fmt.Sprintf(pre("set-window-content: no window found for %v"), z3.Str(a[0])))
		}
		canvas, ok := get(a[1])
		if !ok {
			panic(fmt.Sprintf(pre("set-window-content: no canvas object found for %v"), z3.Str(a[1])))
		}
		win.(fyne.Window).SetContent(canvas.(fyne.CanvasObject))
		return z3.Void
	})

	// (get-window-content <win>) => content ID
	interp.Def(pre("get-window-content"), 1, func(a []any) any {
		win := mustGet(pre("get-window-content"), "GUI window ID", a, 0).(fyne.Window)
		content := win.Content()
		id, ok := getID(content)
		if !ok {
			return put(content)
		}
		return id
	})

	// (set-window-size <window> <width> <height>)
	interp.Def(pre("set-window-size"), 3, func(a []any) any {
		win := mustGet(pre("set-window-size"), "GUI window ID", a, 0).(fyne.Window)
		win.Resize(fyne.NewSize(float32(z3.ToFloat64(a[1])), float32(z3.ToFloat64(a[2]))))
		return z3.Void
	})

	// (close-window <window>)
	interp.Def(pre("close-window"), 1, func(a []any) any {
		win := mustGet(pre("close-window"), "GUI window ID", a, 0)
		win.(fyne.Window).Close()
		clear(win)
		return z3.Void
	})

	// (show-window <window>)
	interp.Def(pre("show-window"), 1, func(a []any) any {
		win := mustGet(pre("show-window"), "GUI window ID", a, 0)
		win.(fyne.Window).Show()
		return z3.Void
	})

	// (hide-window <window>)
	interp.Def(pre("hide-window"), 1, func(a []any) any {
		win := mustGet(pre("hide-window"), "GUI window ID", a, 0)
		win.(fyne.Window).Hide()
		return z3.Void
	})

	// (set-window-on-close-callback <window> <callback>)
	interp.Def(pre("set-window-on-close-callback"), 2, func(a []any) any {
		win := mustGet(pre("set-window-on-close-callback"), "GUI window ID", a, 0)
		proc := a[1].(*z3.Closure)
		win.(fyne.Window).SetOnClosed(func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		return z3.Void
	})

	// (get-window-canvas <win>) => canvas ID
	interp.Def(pre("get-window-canvas"), 1, func(a []any) any {
		win := mustGet(pre("get-window-canvas"), "GUI window ID", a, 0).(fyne.Window)
		canvas := win.Canvas()
		id, ok := getID(canvas)
		if ok {
			return id
		}
		return put(canvas)
	})

	// (get-window-title <win>) => str
	interp.Def(pre("get-window-title"), 1, func(a []any) any {
		win := mustGet(pre("get-window-title"), "GUI window ID", a, 0).(fyne.Window)
		return win.Title()
	})

	// (set-window-title <win> <str>)
	interp.Def(pre("set-window-title"), 2, func(a []any) any {
		win := mustGet(pre("set-window-title"), "GUI window ID", a, 0).(fyne.Window)
		win.SetTitle(a[1].(string))
		return z3.Void
	})

	// (set-window-full-screen <win> <bool>)
	interp.Def(pre("set-window-full-screen"), 2, func(a []any) any {
		win := mustGet(pre("set-window-full-screen"), "GUI window ID", a, 0).(fyne.Window)
		win.SetFullScreen(z3.ToBool(a[1]))
		return z3.Void
	})

	// (window-full-screen? <win>) => bool
	interp.Def(pre("window-full-screen?"), 1, func(a []any) any {
		win := mustGet(pre("window-full-screen?"), "GUI window ID", a, 0).(fyne.Window)
		return z3.AsLispBool(win.FullScreen())
	})

	// (request-window-focus <win>)
	interp.Def(pre("request-window-focus"), 1, func(a []any) any {
		win := mustGet(pre("request-window-focus"), "GUI window ID", a, 0).(fyne.Window)
		win.RequestFocus()
		return z3.Void
	})

	// (set-window-fixed-size <win> <bool>)
	interp.Def(pre("set-window-fized-size"), 2, func(a []any) any {
		win := mustGet(pre("set-window-fixed-size"), "GUI window ID", a, 0).(fyne.Window)
		win.SetFixedSize(z3.ToBool(a[1]))
		return z3.Void
	})

	// (window-fixed-size? <win>) => bool
	interp.Def(pre("window-fixed-size?"), 1, func(a []any) any {
		win := mustGet(pre("window-fixed-size?"), "GUI window ID", a, 0).(fyne.Window)
		return z3.AsLispBool(win.FixedSize())
	})

	// (center-window-on-screen <win>)
	interp.Def(pre("center-window-on-screen"), 1, func(a []any) any {
		win := mustGet(pre("center-window-on-screen"), "GUI window ID", a, 0).(fyne.Window)
		win.CenterOnScreen()
		return z3.Void
	})

	// (set-window-padded <win> <bool>)
	interp.Def(pre("set-window-padded"), 2, func(a []any) any {
		win := mustGet(pre("set-window-padded"), "GUI window ID", a, 0).(fyne.Window)
		win.SetPadded(z3.ToBool(a[1]))
		return z3.Void
	})

	// (window-padded? <win>) => bool
	interp.Def(pre("window-padded?"), 1, func(a []any) any {
		win := mustGet(pre("window-padded?"), "GUI window ID", a, 0).(fyne.Window)
		return z3.AsLispBool(win.Padded())
	})

	// (set-window-icon <win> <icon>)
	interp.Def(pre("set-window-icon"), 2, func(a []any) any {
		win := mustGet(pre("set-window-icon"), "GUI window ID", a, 0).(fyne.Window)
		icon := mustGet(pre("set-window-icon"), "GUI window ID", a, 1).(fyne.Resource)
		win.SetIcon(icon)
		return z3.Void
	})

	// (get-window-icon <win>)
	interp.Def(pre("get-window-icon"), 1, func(a []any) any {
		win := mustGet(pre("get-window-icon"), "GUI window ID", a, 0).(fyne.Window)
		icon := win.Icon()
		id, ok := getID(icon)
		if ok {
			return id
		}
		return put(icon)
	})

	// (set-window-main-menu <win> <menu>)
	interp.Def(pre("set-window-main-menu"), 2, func(a []any) any {
		win := mustGet(pre("set-window-main-menu"), "GUI window ID", a, 0).(fyne.Window)
		menu := mustGet(pre("set-window-main-menu"), "GUI main menu ID", a, 1).(*fyne.MainMenu)
		win.SetMainMenu(menu)
		return z3.Void
	})

	// (get-window-main-menu <win>) => main menu ID
	interp.Def(pre("get-window-main-menu"), 1, func(a []any) any {
		win := mustGet(pre("get-window-main-menu"), "GUI window ID", a, 0).(fyne.Window)
		menu := win.MainMenu()
		id, ok := getID(menu)
		if ok {
			return id
		}
		return put(menu)
	})

	// LABEL

	// (new-label <string>)
	interp.Def(pre("new-label"), 1, func(a []any) any {
		return put(widget.NewLabel(a[0].(string)))
	})

	// (set-label-text <label> <string>)
	interp.Def(pre("set-label-text"), 2, func(a []any) any {
		label := mustGet(pre("set-label-text"), "GUI label ID", a, 0)
		label.(*widget.Label).SetText(a[1].(string))
		return z3.Void
	})

	// ENTRY

	// (new-entry [<selector>])
	interp.Def(pre("new-entry"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		sort := "single-line"
		if li != z3.Nil {
			if s, ok := li.Car.(string); ok {
				sort = s
			} else {
				sort = li.Car.(*z3.Sym).String()
			}
		}
		switch sort {
		case "single", "single-line":
			return put(widget.NewEntry())
		case "multi", "multi-line":
			return put(widget.NewMultiLineEntry())
		case "password":
			return put(widget.NewPasswordEntry())
		default:
			panic(fmt.Sprintf("new-entry: unknown selector '%v, must be one of '(single-line multi-line password)",
				sort))
		}
	})

	// (set-entry-on-change-callback <entry> (lambda (str) ...))
	interp.Def(pre("set-entry-on-change-callback"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-on-change-callback"), "GUI entry ID", a, 0)
		proc := a[1].(*z3.Closure)
		e.(*widget.Entry).OnChanged = func(s string) {
			li2 := &z3.Cell{Car: s, Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		}
		return z3.Void
	})

	// (set-entry-text-wrap entry selector)
	interp.Def(pre("set-entry-text-wrap"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-text-wrap"), "GUI entry ID", a, 0).(*widget.Entry)
		wrap := a[1].(*z3.Sym)
		mode := MustConvertSymToTextWrap(pre("set-entry-text-wrap"), wrap)
		e.Wrapping = mode
		return z3.Void
	})

	// (set-entry-validator <entry> <validator>))
	interp.Def(pre("set-entry-validator"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-validator"), "GUI entry ID", a, 0).(*widget.Entry)
		validator := mustGet(pre("set-entry-validator"), "GUI validator ID", a, 1).(*validatorWrapper)
		e.Validator = validator.fn
		return z3.Void
	})

	// (entry-accepts-tab? <entry>) => bool
	interp.Def(pre("entry-accepts-tab?"), 1, func(a []any) any {
		e := mustGet(pre("entry-accepts-tab"), "entry", a, 0).(*widget.Entry)
		return z3.AsLispBool(e.AcceptsTab())
	})

	// (get-entry-cursor-pos <entry>) => li
	interp.Def(pre("get-entry-cursor-pos"), 1, func(a []any) any {
		e := mustGet(pre("get-entry-cursor-pos"), "GUI entry ID", a, 0).(*widget.Entry)
		row := e.CursorRow
		column := e.CursorColumn
		return &z3.Cell{Car: goarith.AsNumber(row), Cdr: &z3.Cell{Car: goarith.AsNumber(column), Cdr: z3.Nil}}
	})

	// (set-entry-cursor-row <entry> <row>)
	interp.Def(pre("set-entry-cursor-row"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-cursor-row"), "GUI entry ID", a, 0).(*widget.Entry)
		n := z3.ToInt64(pre("set-entry-cursor-row"), a[1])
		e.CursorRow = int(n)
		return z3.Void
	})

	// (set-entry-cursor-column <entry> <column>)
	interp.Def(pre("set-entry-cursor-column"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-cursor-column"), "GUI entry ID", a, 0).(*widget.Entry)
		n := z3.ToInt64(pre("set-entry-cursor-column"), a[1])
		e.CursorColumn = int(n)
		return z3.Void
	})

	// (set-entry-on-cursor-change-callback <entry> <proc>) where <proc> takes an entry ID as argument
	interp.Def(pre("set-entry-on-cursor-change-callback"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-on-cursor-change-callback"), "GUI entry ID", a, 0).(*widget.Entry)
		proc := a[1].(*z3.Closure)
		e.OnCursorChanged = func() {
			interp.Eval(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: a[0], Cdr: z3.Nil}}, z3.Nil)
		}
		return z3.Void
	})

	// (get-entry-cursor <entry>) => sym
	interp.Def(pre("get-entry-cursor"), 1, func(a []any) any {
		e := mustGet(pre("get-entry-cursor"), "entry", a, 0).(*widget.Entry)
		return CursorToSym(e.Cursor())
	})

	// (get-entry-selected-text <entry>) => str
	interp.Def(pre("get-entry-selected-text"), 1, func(a []any) any {
		e := mustGet(pre("get-entry-selected-text"), "entry", a, 0).(*widget.Entry)
		return e.SelectedText()
	})

	// (set-entry-min-rows-visible <entry> <row>)
	interp.Def(pre("set-entry-min-rows-visible"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-min-rows-visible"), "entry", a, 0).(*widget.Entry)
		n := z3.ToInt64(pre("set-entry-min-rows-visible"), a[1])
		e.SetMinRowsVisible(int(n))
		return z3.Void
	})

	// (set-entry-place-holder <entry> <str>)
	interp.Def(pre("set-entry-place-holder"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-place-holder"), "entry", a, 0).(*widget.Entry)
		e.SetPlaceHolder(a[1].(string))
		return z3.Void
	})

	// (set-entry-text <entry> <str>)
	interp.Def(pre("set-entry-text"), 2, func(a []any) any {
		e := mustGet(pre("set-entry-text"), "entry", a, 0).(*widget.Entry)
		e.SetText(a[1].(string))
		return z3.Void
	})

	// VALIDATORS

	// (new-combined-string-validators <validator> [<validators>...]) => validator ID
	interp.Def(pre("new-combined-string-validator"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		validators := make([]fyne.StringValidator, 0)
		for li != z3.Nil {
			validator := mustGet1(pre("new-combined-string-validator"), "GUI string validator ID", li.Car)
			validators = append(validators, validator.(fyne.StringValidator))
			li = li.CdrCell()
		}
		return put(&validatorWrapper{fn: validation.NewAllStrings(validators...)})
	})

	// (new-regexp-validator <regexp-str> <reason>) => validator ID
	interp.Def(pre("new-regexp-validator"), 2, func(a []any) any {
		return put(&validatorWrapper{validation.NewRegexp(a[0].(string), a[1].(string))})
	})

	// (new-time-validator <time-format-str>) => validator ID
	interp.Def(pre("new-time-validator"), 1, func(a []any) any {
		return put(&validatorWrapper{fn: validation.NewTime(a[0].(string))})
	})

	// (new-validator <proc>) => validator ID, where <proc> is a function that takes a string
	// and returns a string. If the return string is not "", the validation fails with the reason
	// given in the string. If <proc> panics, validation also fails.
	interp.Def(pre("new-validator"), 1, func(a []any) any {
		proc := a[0].(*z3.Closure)
		fn := func(s string) error {
			result, err := interp.SafeEval(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: s, Cdr: z3.Nil}}, z3.Nil)
			if err != nil {
				return err.(error)
			}
			str, ok := result.(string)
			if !ok {
				return nil
			}
			if str != "" {
				return errors.New(str)
			}
			return nil
		}
		return put(&validatorWrapper{fn: fyne.StringValidator(fn)})
	})

	// (set-object-on-validation-change-callback <obj> <proc>) where <proc> takes a string as error message
	// and this argument is Nil if validation succeeds.
	interp.Def(pre("set-object-on-validation-change-callback"), 2, func(a []any) any {
		e := mustGet(pre("set-object-on-validation-change-callback"), "GUI validatable ID", a, 0).(fyne.Validatable)
		proc := a[1].(*z3.Closure)
		e.SetOnValidationChanged(func(e error) {
			var arg any
			if e == nil {
				arg = z3.Nil
			} else {
				arg = e.Error()
			}
			interp.Eval(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: arg, Cdr: z3.Nil}}, z3.Nil)
		})
		return z3.Void
	})

	// (validate-object <obj>) => error message string or ""
	interp.Def(pre("validate-object"), 1, func(a []any) any {
		e := mustGet(pre("validate-object"), "GUI validatable ID", a, 0).(fyne.Validatable)
		err := e.Validate()
		if err == nil {
			return ""
		}
		return err.Error()
	})

	// TEXTGRID

	// (new-text-grid [<string>] [show-line-numbers|show-whitespace|tab-width <int>]) => int
	interp.Def(pre("new-text-grid"), -1, func(a []any) any {
		grid := widget.NewTextGrid()
		li := a[0].(*z3.Cell)
		if li != z3.Nil {
			for li != z3.Nil {
				if s, ok := li.Car.(string); ok {
					grid.SetText(s)
				} else if sym, ok := li.Car.(*z3.Sym); ok {
					switch sym.String() {
					case "show-line-numbers":
						grid.ShowLineNumbers = true
					case "show-whitespace":
						grid.ShowWhitespace = true
					case "tab-width":
						li = li.CdrCell()
						if li == z3.Nil {
							panic(pre("new-text-grid: expected a width integer after 'tab-width, but it is missing!"))
						}
						grid.TabWidth = int(z3.ToInt64(pre("new-text-grid"), li.Car))
					}
				} else {
					panic(fmt.Sprintf(pre("new-text-grid: expected an initial string as content and/or a selector in '(show-line-numbers show-whitespace tab-width), given %v"),
						z3.Str(li.Car)))
				}
				li = li.CdrCell()
			}
		}
		return put(grid)
	})

	// (text-grid-show-line-numbers? <grid>) => bool
	interp.Def(pre("text-grid-show-line-numbers?"), 1, func(a []any) any {
		grid := mustGet(pre("text-grid-show-line-numbers?"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		return z3.AsLispBool(grid.ShowLineNumbers)
	})

	// (text-grid-show-whitespace? <grid>) => bool
	interp.Def(pre("text-grid-show-whitespace?"), 1, func(a []any) any {
		grid := mustGet(pre("text-grid-show-whitespace?"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		return z3.AsLispBool(grid.ShowWhitespace)
	})

	// (text-grid-tab-width <grid>) => num
	interp.Def(pre("get-text-grid-tab-width"), 1, func(a []any) any {
		grid := mustGet(pre("get-text-grid-tab-width"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		return goarith.AsNumber(grid.TabWidth)
	})

	// (set-text-grid-tab-width <grid> <n>)
	interp.Def(pre("set-text-grid-tab-width"), 2, func(a []any) any {
		grid := mustGet(pre("set-text-grid-tab-width"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		grid.TabWidth = int(z3.ToInt64(pre("set-text-grid-tab-width"), a[1]))
		return z3.Void
	})

	// (set-text-grid-show-line-numbers <grid> <show?>)
	interp.Def(pre("set-text-grid-show-line-numbers"), 2, func(a []any) any {
		grid := mustGet(pre("set-text-grid-show-line-numbers"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		grid.ShowLineNumbers = z3.ToBool(a[1])
		return z3.Void
	})

	// (set-text-grid-show-whitespace <grid> <show?>)
	interp.Def(pre("set-text-grid-show-whitespace"), 2, func(a []any) any {
		grid := mustGet(pre("set-text-grid-show-whitespace"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		grid.ShowWhitespace = z3.ToBool(a[1])
		return z3.Void
	})

	// (get-text-grid-row <grid> <row>) => li
	// The list consists of an array of cells and a style list. The array of cells contains lists
	// consisting of a rune string and a style list. Style lists consist of a list of foreground color
	// values and a list of background color values.
	interp.Def(pre("get-text-grid-row"), 2, func(a []any) any {
		grid := mustGet(pre("get-text-grid-row"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := grid.Row(int(z3.ToInt64(pre("get-text-grid-row"), a[1])))
		cells := make([]any, 0)
		for _, cell := range row.Cells {
			elem := &z3.Cell{Car: string(cell.Rune), Cdr: &z3.Cell{Car: TextGridStyleToList(cell.Style),
				Cdr: z3.Nil}}
			cells = append(cells, elem)
		}
		return &z3.Cell{Car: cells, Cdr: &z3.Cell{Car: TextGridStyleToList(row.Style), Cdr: z3.Nil}}
	})

	// (get-text-grid-row-text <grid> <row>) => str
	interp.Def(pre("get-text-grid-row-text"), 2, func(a []any) any {
		grid := mustGet(pre("get-text-grid-row-text"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		s := grid.RowText(int(z3.ToInt64(pre("get-text-grid-row-text"), a[1])))
		return s
	})

	// (set-text-grid-cell <grid> <row> <column> <cell>) where <cell> is a list consisting
	// of a rune string and a text grid style list.
	interp.Def(pre("set-text-grid-cell"), 4, func(a []any) any {
		grid := mustGet(pre("set-text-grid-cell"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(pre("set-text-grid-cell"), a[1]))
		column := int(z3.ToInt64(pre("set-text-grid-cell"), a[2]))
		li := a[3].(*z3.Cell)
		r := []rune(li.Car.(string))[0]
		li = li.CdrCell()
		style := ListToTextGridStyle(li)
		grid.SetCell(row, column, widget.TextGridCell{Rune: r, Style: style})
		return z3.Void
	})

	// (get-text-grid-cell <grid> <row> <column>) => li
	interp.Def(pre("get-text-grid-cell"), 3, func(a []any) any {
		grid := mustGet(pre("get-text-grid-cell"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(pre("get-text-grid-cell"), a[1]))
		column := int(z3.ToInt64(pre("get-text-grid-cell"), a[2]))
		cell := grid.Rows[row].Cells[column]
		return &z3.Cell{Car: string(cell.Rune), Cdr: &z3.Cell{Car: TextGridStyleToList(cell.Style),
			Cdr: z3.Nil}}
	})

	// (set-text-grid-row <grid> <row> <rowspec>) where <rowspec> has the same format as is returned
	// by get-text-grid-row, i.e. it is an array of cell lists containing a rune string and a style list.
	interp.Def(pre("set-text-grid-row"), 3, func(a []any) any {
		grid := mustGet(pre("set-text-grid-row"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(pre("set-text-grid-row"), a[1]))
		li := a[2].(*z3.Cell)
		columns := li.Car.([]any)
		li = li.CdrCell()
		style := ListToTextGridStyle(li.Car.(*z3.Cell))
		cells := make([]widget.TextGridCell, 0, len(columns))
		for i := range columns {
			li = columns[i].(*z3.Cell)
			r := []rune(li.Car.(string))[0]
			li = li.CdrCell()
			sty := ListToTextGridStyle(li.Car.(*z3.Cell))
			cells = append(cells, widget.TextGridCell{Rune: r, Style: sty})
		}
		grid.SetRow(row, widget.TextGridRow{Cells: cells, Style: style})
		return z3.Void
	})

	// (set-text-grid-row-style <grid> <row> <style-list>) sets the whole row to the style list,
	// which contains a foreground color and a background color list.
	interp.Def(pre("set-text-grid-row-style"), 3, func(a []any) any {
		grid := mustGet(pre("set-text-grid-row-style"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(pre("set-text-grid-row-style"), a[1]))
		li := a[2].(*z3.Cell)
		style := ListToTextGridStyle(li)
		grid.SetRowStyle(row, style)
		return z3.Void
	})

	// (set-text-grid-rune <grid> <row> <column> <str>)
	interp.Def(pre("set-text-grid-rune"), 4, func(a []any) any {
		grid := mustGet(pre("set-text-grid-rune"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(pre("set-text-grid-rune"), a[1]))
		column := int(z3.ToInt64(pre("set-text-grid-rune"), a[2]))
		r := []rune(a[3].(string))[0]
		grid.SetRune(row, column, r)
		return z3.Void
	})

	// (set-text-grid-style <grid> <row> <column> <style-list>)
	interp.Def(pre("set-text-grid-style"), 4, func(a []any) any {
		grid := mustGet(pre("set-text-grid-style"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(pre("set-text-grid-style"), a[1]))
		column := int(z3.ToInt64(pre("set-text-grid-style"), a[2]))
		style := ListToTextGridStyle(a[3].(*z3.Cell))
		grid.SetStyle(row, column, style)
		return z3.Void
	})

	// (set-text-grid-style-range <grid> <start-row> <start-column> <end-row> <end-column> <style-list>)
	interp.Def(pre("set-text-grid-style-range"), 6, func(a []any) any {
		grid := mustGet(pre("set-text-grid-style-range"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		row1 := int(z3.ToInt64(pre("set-text-grid-style-range"), a[1]))
		column1 := int(z3.ToInt64(pre("set-text-grid-style-range"), a[2]))
		row2 := int(z3.ToInt64(pre("set-text-grid-style-range"), a[3]))
		column2 := int(z3.ToInt64(pre("set-text-grid-style-range"), a[4]))
		style := ListToTextGridStyle(a[5].(*z3.Cell))
		grid.SetStyleRange(row1, column1, row2, column2, style)
		return z3.Void
	})

	// (set-text-grid-text <grid> <str>)
	interp.Def(pre("set-text-grid-text"), 2, func(a []any) any {
		grid := mustGet(pre("set-text-grid-text"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		s := a[1].(string)
		grid.SetText(s)
		return z3.Void
	})

	// (get-text-grid-text <grid>) => str
	interp.Def(pre("get-text-grid-text"), 1, func(a []any) any {
		grid := mustGet(pre("get-text-grid-text"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		return grid.Text()
	})

	// CHECK

	// (new-check <title-string> (lambda (bool) ...))
	interp.Def(pre("new-check"), 2, func(a []any) any {
		title := a[0].(string)
		proc := a[1].(*z3.Closure)
		changed := func(b bool) {
			li2 := &z3.Cell{Car: z3.AsLispBool(b), Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		}
		return put(widget.NewCheck(title, changed))
	})

	// CHOICE

	// (new-choice <selector> <string-list> (lambda (str) ...))
	interp.Def(pre("new-choice"), 3, func(a []any) any {
		sort := "select"
		if s, ok := a[0].(string); ok {
			sort = s
		} else {
			sort = a[0].(*z3.Sym).String()
		}
		li := a[1].(*z3.Cell)
		proc := a[2].(*z3.Closure)
		changed := func(s string) {
			li2 := &z3.Cell{Car: s, Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		}
		s := make([]string, 0)
		for li != z3.Nil {
			s = append(s, li.Car.(string))
			li = li.CdrCell()
		}
		switch sort {
		case "select":
			return put(widget.NewSelect(s, changed))
		case "radio", "radio-group":
			return put(widget.NewRadioGroup(s, changed))
		default:
			panic(fmt.Sprintf("new-choice: the first argument must be one of '(select radio-group), given %v", sort))
		}
	})

	// FORM

	// (new-form)
	interp.Def(pre("new-form"), 0, func(a []any) any {
		return put(widget.NewForm())
	})

	// (append-form <form> <string> <canvas-object>)
	interp.Def(pre("append-form"), 3, func(a []any) any {
		form := mustGet(pre("append-form"), "form", a, 0)
		obj := mustGet(pre("append-form"), "canvas-object", a, 2)
		form.(*widget.Form).Append(a[1].(string), obj.(fyne.CanvasObject))
		return z3.Void
	})

	// HYPERLINK

	// (new-hyperlink label url) => int
	interp.Def(pre("new-hyperlink"), 2, func(a []any) any {
		if !cfg.HyperlinksAllowed {
			panic(pre("new-hyperlink: hyperlinks are not permitted!"))
		}
		url, err := url.Parse(a[1].(string))
		if err != nil {
			panic(fmt.Errorf(pre(`new-hyperlink: %w, given %v`), err, a[1].(string)))
		}
		if cfg.CheckHyperlinks != nil {
			url = cfg.CheckHyperlinks(url)
		}
		if url == nil {
			panic(fmt.Sprintf(pre(`new-hyperlink:the link URL "%v" is not permitted!`), a[1].(string)))
		}
		return put(widget.NewHyperlink(a[0].(string), url))
	})

	// BUTTON

	// (new-button str proc) => int
	interp.Def(pre("new-button"), 2, func(a []any) any {
		proc := a[1].(*z3.Closure)
		b := widget.NewButton(a[0].(string), func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		return put(b)
	})

	interp.Def(pre("new-button-with-icon"), 3, func(a []any) any {
		icon := mustGet(pre("new-button-with-icon"), "GUI icon resource ID", a, 1).(fyne.Resource)
		proc := a[2].(*z3.Closure)
		b := widget.NewButtonWithIcon(a[0].(string), icon, func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		return put(b)
	})

	// LIST

	// (new-list <len-proc> <prepare-proc> <update-proc>)
	interp.Def(pre("new-list"), 3, func(a []any) any {
		lproc := a[0].(*z3.Closure)
		pproc := a[1].(*z3.Closure)
		uproc := a[2].(*z3.Closure)
		lenCB := func() int {
			n := interp.Eval(&z3.Cell{Car: lproc, Cdr: z3.Nil}, z3.Nil)
			return int(z3.ToInt64("GUI list length callback", n))
		}
		prepCB := func() fyne.CanvasObject {
			result := interp.Eval(&z3.Cell{Car: pproc, Cdr: z3.Nil}, z3.Nil)
			obj := mustGet1(pre("new-list"), "GUI canvas object ID (result from list preparation callback)", result)
			return obj.(fyne.CanvasObject)
		}
		updCB := func(i widget.ListItemID, o fyne.CanvasObject) {
			n := int(i)
			if id, ok := getID(o); ok {
				interp.Eval(&z3.Cell{Car: uproc, Cdr: &z3.Cell{Car: id, Cdr: &z3.Cell{Car: goarith.AsNumber(n), Cdr: z3.Nil}}}, z3.Nil)
			}
		}
		return put(widget.NewList(lenCB, prepCB, updCB))
	})

	// TABLE

	// (new-table <len-proc> <prepare-proc> <update-proc>)
	interp.Def(pre("new-table"), 3, func(a []any) any {
		lproc := a[0].(*z3.Closure)
		pproc := a[1].(*z3.Closure)
		uproc := a[2].(*z3.Closure)
		lenCB := func() (int, int) {
			c := interp.Eval(&z3.Cell{Car: lproc, Cdr: z3.Nil}, z3.Nil).(*z3.Cell)
			a := z3.ToInt64("GUI table length callback return element #1", c.Car)
			c = c.CdrCell()
			b := z3.ToInt64("GUI table length callback return element #2", c.Car)
			return int(a), int(b)
		}
		prepCB := func() fyne.CanvasObject {
			result := interp.Eval(&z3.Cell{Car: pproc, Cdr: z3.Nil}, z3.Nil)
			obj := mustGet1(pre("new-table"), "GUI canvas object ID (result from table preparation callback)", result)
			return obj.(fyne.CanvasObject)
		}
		updCB := func(id widget.TableCellID, o fyne.CanvasObject) {
			row := id.Row
			col := id.Col
			if id, ok := getID(o); ok {
				interp.Eval(&z3.Cell{Car: uproc, Cdr: &z3.Cell{Car: goarith.AsNumber(id), Cdr: &z3.Cell{Car: goarith.AsNumber(row), Cdr: &z3.Cell{Car: goarith.AsNumber(col), Cdr: z3.Nil}}}}, z3.Nil)
			}
		}
		return put(widget.NewTable(lenCB, prepCB, updCB))
	})

	// TREE

	// (new-tree <child-uid-proc> <is-branch-proc> <create-node-proc> <update-note-proc)
	interp.Def(pre("new-tree"), 4, func(a []any) any {
		proc1 := a[0].(*z3.Closure)
		proc2 := a[1].(*z3.Closure)
		proc3 := a[2].(*z3.Closure)
		proc4 := a[3].(*z3.Closure)
		fn1 := func(id widget.TreeNodeID) []widget.TreeNodeID {
			var s string = id
			li := interp.Eval(&z3.Cell{Car: proc1, Cdr: &z3.Cell{Car: s, Cdr: z3.Nil}}, z3.Nil).(*z3.Cell)
			arr := make([]widget.TreeNodeID, 0)
			for li != z3.Nil {
				arr = append(arr, widget.TreeNodeID(li.Car.(string)))
				li = li.CdrCell()
			}
			return arr
		}
		fn2 := func(id widget.TreeNodeID) bool {
			var s string = id
			result := interp.Eval(&z3.Cell{Car: proc2, Cdr: &z3.Cell{Car: s, Cdr: z3.Nil}}, z3.Nil)
			return z3.ToBool(result)
		}
		fn3 := func(branch bool) fyne.CanvasObject {
			result := interp.Eval(&z3.Cell{Car: proc3, Cdr: &z3.Cell{Car: z3.AsLispBool(branch), Cdr: z3.Nil}}, z3.Nil)
			obj := mustGet1(pre("new-tree"), "GUI tree creation callback canvas object ID", result)
			return obj.(fyne.CanvasObject)
		}
		fn4 := func(id widget.TreeNodeID, branch bool, o fyne.CanvasObject) {
			var s string = id
			objID, _ := getID(o)
			interp.Eval(&z3.Cell{Car: proc4, Cdr: &z3.Cell{Car: s, Cdr: &z3.Cell{Car: z3.AsLispBool(branch), Cdr: &z3.Cell{Car: objID, Cdr: z3.Nil}}}}, z3.Nil)
		}
		return put(widget.NewTree(fn1, fn2, fn3, fn4))
	})

	// MENU

	// (new-menu-item <str> <proc> [<selector>...]) => menu item ID
	interp.Def(pre("new-menu-item"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		label := li.Car.(string)
		li = li.CdrCell()
		proc := li.Car.(*z3.Closure)
		li = li.CdrCell()
		item := fyne.NewMenuItem(label, func() {
			interp.Eval(&z3.Cell{Car: proc, Cdr: z3.Nil}, z3.Nil)
		})
		for li != z3.Nil {
			sym := li.Car.(*z3.Sym)
			switch sym {
			case IsQuitSym:
				item.IsQuit = true
			case IsSeparatorSym:
				item.IsSeparator = true
			case DisabledSym:
				item.Disabled = true
			case CheckedSym:
				item.Checked = true
			default:
				panic(fmt.Sprintf(pre("new-menu-item: expected a symbol in '(is-quit is-separator disabled checked) but given %v"), z3.Str(li.Car)))
			}
			li = li.CdrCell()
		}
		return put(item)
	})

	// (set-menu-item-checked <item> <bool>)
	interp.Def(pre("set-menu-item-checked"), 2, func(a []any) any {
		item := mustGet(pre("set-menu-item-checked"), "GUI menu item ID", a, 0).(*fyne.MenuItem)
		b := z3.ToBool(a[1])
		item.Checked = b
		return z3.Void
	})

	// (menu-item-checked? <item>) => bool
	interp.Def(pre("menu-item-checked?"), 1, func(a []any) any {
		item := mustGet(pre("menu-item-checked?"), "GUI menu item ID", a, 0).(*fyne.MenuItem)
		return z3.AsLispBool(item.Checked)
	})

	// (set-menu-item-disabled <item> <bool>)
	interp.Def(pre("set-menu-item-disabled"), 2, func(a []any) any {
		item := mustGet(pre("set-menu-item-disabled"), "GUI menu item ID", a, 0).(*fyne.MenuItem)
		b := z3.ToBool(a[1])
		item.Disabled = b
		return z3.Void
	})

	// (menu-item-disabled? <item>) => bool
	interp.Def(pre("menu-item-disabled?"), 1, func(a []any) any {
		item := mustGet(pre("menu-item-disabled?"), "GUI menu item ID", a, 0).(*fyne.MenuItem)
		return z3.AsLispBool(item.Disabled)
	})

	// (get-menu-item-label <item>) => str
	interp.Def(pre("get-menu-item-label"), 1, func(a []any) any {
		item := mustGet(pre("get-menu-item-label"), "GUI menu item ID", a, 0).(*fyne.MenuItem)
		return item.Label
	})

	// (set-menu-item-label <item> <str>)
	interp.Def(pre("set-menu-item-label"), 2, func(a []any) any {
		item := mustGet(pre("set-menu-item-label"), "GUI menu item ID", a, 0).(*fyne.MenuItem)
		item.Label = a[1].(string)
		return z3.Void
	})

	// (new-menu-item-separator) => menu item ID
	interp.Def(pre("new-menu-item-separator"), 0, func(a []any) any {
		return put(fyne.NewMenuItemSeparator())
	})

	// (new-menu* <str> [<item>...]) => menu* ID
	interp.Def(pre("new-menu*"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		label := li.Car.(string)
		li = li.CdrCell()
		items := make([]*fyne.MenuItem, 0)
		fname := pre("new-menu*")
		for li != z3.Nil {
			item := mustGet1(fname, "GUI menu item ID", li.Car).(*fyne.MenuItem)
			items = append(items, item)
			li = li.CdrCell()
		}
		return put(fyne.NewMenu(label, items...))
	})

	// (refresh-menu* <menu>)
	interp.Def(pre("refresh-menu*"), 1, func(a []any) any {
		menu := mustGet(pre("refresh-menu"), "GUI menu item ID", a, 0).(*fyne.Menu)
		menu.Refresh()
		return z3.Void
	})

	// (new-menu <menu>) => menu ID
	interp.Def(pre("new-menu"), 1, func(a []any) any {
		m := mustGet(pre("new-menu"), "GUI menu* ID", a, 0).(*fyne.Menu)
		return put(widget.NewMenu(m))
	})

	// (activate-menu-last-submenu <menu>) => bool
	interp.Def(pre("activate-menu-last-submenu"), 1, func(a []any) any {
		menu := mustGet(pre("activate-menu-last-submenu"), "GUI menu ID", a, 0).(*widget.Menu)
		return z3.AsLispBool(menu.ActivateLastSubmenu())
	})

	// (activate-meno u-next <menu>)
	interp.Def(pre("activate-menu-next"), 1, func(a []any) any {
		menu := mustGet(pre("activate-menu-next"), "GUI menu ID", a, 0).(*widget.Menu)
		menu.ActivateNext()
		return z3.Void
	})

	// (activate-menu-previous <menu>)
	interp.Def(pre("activate-menu-previous"), 1, func(a []any) any {
		menu := mustGet(pre("activate-menu-previous"), "GUI menu ID", a, 0).(*widget.Menu)
		menu.ActivatePrevious()
		return z3.Void
	})

	// (deactivate-menu-child <menu>)
	interp.Def(pre("deactivate-menu-child"), 1, func(a []any) any {
		menu := mustGet(pre("deactivate-menu-child"), "GUI menu ID", a, 0).(*widget.Menu)
		menu.DeactivateChild()
		return z3.Void
	})

	// (deactivate-menu-last-submenu <menu>)
	interp.Def(pre("deactivate-menu-last-submenu"), 1, func(a []any) any {
		menu := mustGet(pre("deactivate-menu-last-submenu"), "GUI menu ID", a, 0).(*widget.Menu)
		menu.DeactivateLastSubmenu()
		return z3.Void
	})

	// (trigger-menu-last <menu>)
	interp.Def(pre("trigger-menu-last"), 1, func(a []any) any {
		menu := mustGet(pre("trigger-menu-last"), "GUI menu ID", a, 0).(*widget.Menu)
		menu.TriggerLast()
		return z3.Void
	})

	// (new-main-menu <menu> ...) => main menu ID
	interp.Def(pre("new-main-menu"), -1, func(a []any) any {
		items := make([]*fyne.Menu, 0)
		li := a[0].(*z3.Cell)
		for li != z3.Nil {
			menu := li.Car.(*fyne.Menu)
			items = append(items, menu)
			li = li.CdrCell()
		}
		return put(fyne.NewMainMenu(items...))
	})

	// (refresh-main-menu <menu>)
	interp.Def(pre("refresh-main-menu"), 1, func(a []any) any {
		menu := mustGet(pre("refresh-main-menu"), "GUI main menu ID", a, 0).(*fyne.MainMenu)
		menu.Refresh()
		return z3.Void
	})

	// IMAGE

	// (new-image-from-resource <resource>)
	interp.Def(pre("new-image-from-resource"), 1, func(a []any) any {
		res := mustGet(pre("new-image-from-resource"), "GUI resource ID", a, 0)
		return put(canvas.NewImageFromResource(res.(fyne.Resource)))
	})

	// (new-image-from-file <path-string>)
	interp.Def(pre("new-image-from-file"), 1, func(a []any) any {
		return put(canvas.NewImageFromFile(a[0].(string)))
	})

	// DRAWING

	// color helpers

	// (nrgba r g b a) => NRGBA color
	interp.Def(pre("nrgba"), 4, func(a []any) any {
		r := z3.ToUInt8(a[0])
		g := z3.ToUInt8(a[1])
		b := z3.ToUInt8(a[2])
		alpha := z3.ToUInt8(a[3])
		return put(color.NRGBA{R: r, G: g, B: b, A: alpha})
	})

	// (new-rectangle <fill-color> [<width> <height>] [<position>] [<stroke-color>] [<stroke-width>] [<corner-radius>])
	interp.Def(pre("new-rectangle"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		fillColor := mustGet1(pre("new-rectangle"), "GUI nrgba color ID", li.Car).(color.Color)
		rect := canvas.NewRectangle(fillColor)
		li = li.CdrCell()
		if li == z3.Nil {
			return put(rect)
		}
		w := float32(z3.ToFloat64(li.Car))
		li = li.CdrCell()
		h := float32(z3.ToFloat64(li.Car))
		rect.Resize(fyne.NewSize(w, h))
		li = li.CdrCell()
		if li == z3.Nil {
			return put(rect)
		}
		pos, ok := MustGetPosition(pre("new-rectangle"), 3, li.Car)
		if ok {
			rect.Move(pos)
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(rect)
		}
		strokeColor := mustGet1(pre("new-rectangle"), "GUI nrgba color ID", li.Car).(color.Color)
		rect.StrokeColor = strokeColor
		li = li.CdrCell()
		if li == z3.Nil {
			return put(rect)
		}
		rect.StrokeWidth = float32(z3.ToFloat64(li.Car))
		li = li.CdrCell()
		if li == z3.Nil {
			return put(rect)
		}
		rect.CornerRadius = float32(z3.ToFloat64(li.Car))
		return put(rect)
	})

	interp.Def(pre("set-rectangle-min-size"), 3, func(a []any) any {
		r := mustGet1(pre("set-rectangle-min-size"), "GUI rectangle ID", a[0])
		w := z3.ToFloat64(a[1])
		h := z3.ToFloat64(a[2])
		r.(*canvas.Rectangle).SetMinSize(fyne.NewSize(float32(w), float32(h)))
		return z3.Void
	})

	// (new-circle <fill-color> [<pos1>] [<pos2>] [<stroke-color>] [<stroke-width>])
	interp.Def(pre("new-circle"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		fillColor := mustGet1(pre("new-circle"), "GUI nrgba color ID", li.Car).(color.Color)
		circle := canvas.NewCircle(fillColor)
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		pos1, ok := MustGetPosition(pre("new-circle"), 0, li.Car)
		if ok {
			circle.Position1 = pos1
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		pos2, ok := MustGetPosition(pre("new-circle"), 1, li.Car)
		if ok {
			circle.Position2 = pos2
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		strokeColor := mustGet1(pre("new-circle"), "GUI nrgba color ID", li.Car).(color.Color)
		circle.StrokeColor = strokeColor
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		circle.StrokeWidth = float32(z3.ToFloat64(li.Car))
		return put(circle)
	})

	// (new-line <fill-color> [<pos1>] [<pos2>] [<stroke-color>] [<stroke-width>])
	interp.Def(pre("new-line"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		fillColor := mustGet1(pre("new-line"), "GUI nrgba color ID", li.Car).(color.Color)
		line := canvas.NewLine(fillColor)
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		pos1, ok := MustGetPosition(pre("new-line"), 0, li.Car)
		if ok {
			line.Position1 = pos1
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		pos2, ok := MustGetPosition(pre("new-line"), 1, li.Car)
		if ok {
			line.Position2 = pos2
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		strokeColor := mustGet1(pre("new-line"), "GUI nrgba color ID", li.Car).(color.Color)
		line.StrokeColor = strokeColor
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		line.StrokeWidth = float32(z3.ToFloat64(li.Car))
		return put(line)
	})

	interp.Def(pre("new-text"), 2, func(a []any) any {
		s := a[0].(string)
		color := mustGet1(pre("new-text"), "GUI nrgba color ID", a[1]).(color.Color)
		return put(canvas.NewText(s, color))
	})

	interp.Def(pre("set-text-alignment"), 2, func(a []any) any {
		text := mustGet1(pre("set-text-alignment"), "GUI text ID", a[0])
		align, ok := MustGetTextAlign(pre("set-text-alignment"), 1, a[1])
		if ok {
			text.(*canvas.Text).Alignment = align
		}
		return z3.Void
	})

	interp.Def(pre("set-text-size"), 2, func(a []any) any {
		text := mustGet1(pre("set-text-size"), "GUI text ID", a[0])
		text.(*canvas.Text).TextSize = float32(z3.ToFloat64(a[1]))
		return z3.Void
	})

	interp.Def(pre("set-text-style"), 2, func(a []any) any {
		text := mustGet1(pre("set-text-style"), "GUI text ID", a[0]).(*canvas.Text)
		style := MustGetTextStyle(pre("set-text-style"), 1, a[1])
		text.TextStyle = style
		return z3.Void
	})

	// (new-raster-with-pixels <pixel-proc>) where <pixel-proc> takes x, y, w, h and returns
	// a color list (NOT an nrgba color, for performance reasons this is created at the Go side).
	interp.Def(pre("new-raster-with-pixels"), 1, func(a []any) any {
		proc := a[0].(*z3.Closure)
		raster := canvas.NewRasterWithPixels(func(x, y, w, h int) color.Color {
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: goarith.AsNumber(x), Cdr: &z3.Cell{Car: goarith.AsNumber(y),
				Cdr: &z3.Cell{Car: goarith.AsNumber(w), Cdr: &z3.Cell{Car: goarith.AsNumber(h), Cdr: z3.Nil}}}}}
			result := interp.Eval(li, z3.Nil)
			li = result.(*z3.Cell)
			return ListToColor(li)
		})
		return put(raster)
	})

	// KEYBOARD

	interp.Def(pre("new-shortcut"), -1, func(a []any) any {
		key, modifier := MustGetShortcut(pre("new-shortcut"), a[0].(*z3.Cell))
		return put(&desktop.CustomShortcut{KeyName: key, Modifier: modifier})
	})

	// CANVAS

	// (add-canvas-shortcut canvas shortcut proc)
	interp.Def(pre("add-canvas-shortcut"), 3, func(a []any) any {
		canvas := mustGet(pre("add-canvas-shortcut"), "GUI canvas ID", a, 0).(fyne.Canvas)
		shortcut := mustGet(pre("add-canvas-shortcut"), "GUI shortcut ID", a, 1).(fyne.Shortcut)
		proc := a[2].(*z3.Closure)

		canvas.AddShortcut(shortcut, func(sc fyne.Shortcut) {
			obj, ok := getID(sc)
			if !ok {
				panic(fmt.Sprintf(pre("add-canvas-shortcut: shortcut id not found in handler: %v"), z3.Str(a[1])))
			}
			interp.Eval(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: obj, Cdr: z3.Nil}}, z3.Nil)
		})
		return z3.Void
	})

	// (remove-canvas-shortcut canvas shortcut)
	interp.Def(pre("remove-canvas-shortcut"), 2, func(a []any) any {
		canvas := mustGet(pre("remove-canvas-shortcut"), "GUI canvas ID", a, 0).(fyne.Canvas)
		shortcut := mustGet(pre("remove-canvas-shortcut"), "GUI shortcut ID", a, 1).(fyne.Shortcut)
		canvas.RemoveShortcut(shortcut)
		return z3.Void
	})

	// (set-canvas-on-typed-key canvas proc)
	interp.Def(pre("set-canvas-on-typed-key"), 2, func(a []any) any {
		canvas := mustGet(pre("set-canvas-on-typed-key"), "GUI canvas ID", a, 0).(fyne.Canvas)
		proc := a[1].(*z3.Closure)
		canvas.SetOnTypedKey(func(evt *fyne.KeyEvent) {
			qq := z3.QqQuote(KeyNameToSymbol(evt.Name))
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: qq, Cdr: &z3.Cell{Car: goarith.AsNumber(evt.Physical.ScanCode), Cdr: z3.Nil}}}
			interp.Eval(li, z3.Nil)
		})
		return z3.Void
	})

	// (focus-canvas-object canvas object)
	interp.Def(pre("focus-canvas-object"), 2, func(a []any) any {
		canvas := mustGet(pre("focus-canvas-object"), "GUI canvas ID", a, 0).(fyne.Canvas)
		obj, ok := mustGet(pre("focus-canvas-object"), "GUI focusable canvas object ID", a, 1).(fyne.Focusable)
		if !ok {
			panic(fmt.Sprintf("%v: expected a focusable canvas object as second argument, but the given canvas object cannot take focus: %v", pre("focus-canvas-object"), a[1]))
		}
		canvas.Focus(obj)
		return z3.Void
	})

	// CANVAS OBJECT (polymorphic methods)

	// (disable-object <obj>)
	interp.Def(pre("disable-object"), 2, func(a []any) any {
		obj := mustGet(pre("disable-object"), "GUI disableable object ID", a, 0)
		obj.(fyne.Disableable).Disable()
		return z3.Void
	})

	// (enable-object <obj>)
	interp.Def(pre("enable-object"), 2, func(a []any) any {
		obj := mustGet(pre("enable-object"), "GUI disableable object ID", a, 0)
		obj.(fyne.Disableable).Enable()
		return z3.Void
	})

	// (hide-object <obj>)
	interp.Def(pre("hide-object"), 2, func(a []any) any {
		obj := mustGet(pre("hide-object"), "GUI widget ID", a, 0)
		obj.(fyne.Widget).Hide()
		return z3.Void
	})

	// (show-object <obj>)
	interp.Def(pre("show-object"), 2, func(a []any) any {
		obj := mustGet(pre("show-object"), "GUI widget ID", a, 0)
		obj.(fyne.Widget).Show()
		return z3.Void
	})

	// (object-disabled? <obj>) => bool
	interp.Def(pre("object-disabled?"), 2, func(a []any) any {
		obj := mustGet(pre("object-disabled?"), "GUI disableable object ID", a, 0)
		return z3.AsLispBool(obj.(fyne.Disableable).Disabled())
	})

	// (move-object <obj> <pos>) attempts to move a GUI object (polymorphic)
	interp.Def(pre("move-object"), 2, func(a []any) any {
		obj := mustGet(pre("move-object"), "GUI canvas object ID", a, 0)
		pos, ok := MustGetPosition(pre("gui-move"), 1, a[1])
		if !ok {
			return z3.Void
		}
		obj.(fyne.CanvasObject).Move(pos)
		return z3.Void
	})

	// (resize-object <obj> <w> <h>) attempts to resize a GUI object (polymorphic)
	interp.Def(pre("resize-object"), 3, func(a []any) any {
		obj := mustGet(pre("resize-object"), "GUI canvas object ID", a, 0)
		w := float32(z3.ToFloat64(a[1]))
		h := float32(z3.ToFloat64(a[2]))
		obj.(fyne.CanvasObject).Resize(fyne.NewSize(w, h))
		return z3.Void
	})

	// (get-object-size <obj>) => li
	interp.Def(pre("get-object-size"), 1, func(a []any) any {
		obj := mustGet(pre("get-object-size"), "GUI canvas object ID", a, 0)
		size := obj.(fyne.CanvasObject).Size()
		return &z3.Cell{Car: goarith.AsNumber(float64(size.Width)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(size.Height)), Cdr: z3.Nil}}
	})

	// (get-object-min-size <obj>) => li
	interp.Def(pre("get-object-min-size"), 1, func(a []any) any {
		obj := mustGet(pre("get-object-min-size"), "GUI canvas object ID", a, 0)
		size := obj.(fyne.CanvasObject).MinSize()
		return &z3.Cell{Car: goarith.AsNumber(float64(size.Width)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(size.Height)), Cdr: z3.Nil}}
	})

	// (get-object-position <obj>) => li
	interp.Def(pre("get-object-position"), 1, func(a []any) any {
		obj := mustGet(pre("get-object-position"), "GUI canvas object ID", a, 0)
		pos := obj.(fyne.CanvasObject).Position()
		return &z3.Cell{Car: goarith.AsNumber(float64(pos.X)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(pos.Y)), Cdr: z3.Nil}}
	})

	// (object-visible? <obj>) => bool
	interp.Def(pre("object-visible?"), 1, func(a []any) any {
		obj := mustGet(pre("object-visible?"), "GUI canvas object ID", a, 0)
		return z3.AsLispBool(obj.(fyne.CanvasObject).Visible())
	})

	// (refresh-object <obj>)
	interp.Def(pre("refresh-object"), 1, func(a []any) any {
		obj := mustGet(pre("refresh-object"), "GUI canvas object ID", a, 0)
		obj.(fyne.CanvasObject).Refresh()
		return z3.Void
	})

	// PROGRESSBAR

	// (new-progress-bar)
	interp.Def(pre("new-progress-bar"), 0, func(a []any) any {
		return put(widget.NewProgressBar())
	})

	// (new-infinite-progress-bar)
	interp.Def(pre("new-infinite-progress-bar"), 0, func(a []any) any {
		return put(widget.NewProgressBarInfinite())
	})

	// (set-progress-bar <bar> <value>|[<selector> <value>])
	interp.Def(pre("set-progress-bar"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		bar := mustGet1(pre("set-progress-bar"), "GUI progress-bar ID", li.Car).(*widget.ProgressBar)
		li = li.CdrCell()
		if sym, ok := li.Car.(*z3.Sym); ok {
			li = li.CdrCell()
			switch sym.String() {
			case "value":
				bar.SetValue(z3.ToFloat64(li.Car))
			case "min":
				bar.Min = z3.ToFloat64(li.Car)
			case "max":
				bar.Max = z3.ToFloat64(li.Car)
			case "formatter":
				proc := li.Car.(*z3.Closure)
				id, _ := getID(bar)
				fn := func() string {
					result := interp.Eval(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: id, Cdr: z3.Nil}}, z3.Nil)
					if s, ok := result.(string); ok {
						return s
					}
					panic(fmt.Sprintf(pre("set-progress-bar: formatter callback is expected to return a string but it returned %v"), z3.Str(result)))
				}
				bar.TextFormatter = fn
			default:
				panic(fmt.Sprintf(pre("set-progress-bar: expected selector in '(value min max formatter), given %v"), sym.String()))
			}
			return z3.Void
		}
		bar.SetValue(z3.ToFloat64(li.Car))
		return z3.Void
	})

	// (get-progress-bar-value <bar>)
	interp.Def(pre("get-progress-bar-value"), 1, func(a []any) any {
		bar := mustGet(pre("get-progress-bar-value"), "GUI progress-bar ID", a, 0)
		n := bar.(*widget.ProgressBar).Value
		return goarith.AsNumber(n)
	})

	// SLIDER

	// (new-slider <min> <max> <change-cb>)
	interp.Def(pre("new-slider"), 3, func(a []any) any {
		proc := a[2].(*z3.Closure)
		fn := func(v float64) {
			interp.Eval(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: goarith.AsNumber(v), Cdr: z3.Nil}}, z3.Nil)
		}
		slider := widget.NewSlider(z3.ToFloat64(a[0]), z3.ToFloat64(a[1]))
		slider.OnChanged = fn
		return put(slider)
	})

	// (set-slider-value <slider> <value>)
	interp.Def(pre("set-slider-value"), 2, func(a []any) any {
		slider := mustGet(pre("set-slider-value"), "GUI slider ID", a, 0).(*widget.Slider)
		slider.SetValue(z3.ToFloat64(a[1]))
		return z3.Void
	})

	// ICON

	// (new-icon <resource>)
	interp.Def(pre("new-icon"), 1, func(a []any) any {
		res := mustGet(pre("new-icon"), "GUI resource ID", a, 0)
		return put(widget.NewIcon(res.(fyne.Resource)))
	})

	// MISC

	// (forget-gui-object <id>) clears any internal association with the given GUI object
	// but does not destroy resources associated with it. WARN: Internal use only, use with care!
	interp.Def(pre("forget-gui-object"), 1, func(a []any) any {
		clear(a[0])
		return z3.Void
	})

	// (close-gui) closes the GUI, none of its elements can be used again and the application
	// must shut down all GUI activity. Open windows are closed.
	interp.Def(pre("close-gui"), 0, func(a []any) any {
		CloseGUI()
		return z3.Void
	})

	interp.Def(pre("get-clipboard-content"), 0, func(a []any) any {
		if !config.ClipboardGetAllowed {
			panic("getting clipboard content is prohibited by security policy!")
		}
		s := mainWin.Clipboard().Content()
		return s
	})

	interp.Def(pre("set-clipboard-content"), 1, func(a []any) any {
		if !config.ClipboardSetAllowed {
			panic("setting clipboard content is prohibited by security policy!")
		}
		mainWin.Clipboard().SetContent(a[0].(string))
		return z3.Void
	})

	interp.Def(pre("get-device-info"), 0, func(a []any) any {
		arr := make([]any, 0)
		dev := fyne.CurrentDevice()
		var sym *z3.Sym
		switch dev.Orientation() {
		case fyne.OrientationVertical:
			sym = z3.NewSym("vertical")
		case fyne.OrientationVerticalUpsideDown:
			sym = z3.NewSym("vertical-upside-down")
		case fyne.OrientationHorizontalLeft:
			sym = z3.NewSym("left")
		case fyne.OrientationHorizontalRight:
			sym = z3.NewSym("right")
		default:
			sym = z3.NewSym("unknown")
		}
		arr = append(arr, &z3.Cell{Car: z3.NewSym("orientation"), Cdr: &z3.Cell{Car: sym, Cdr: z3.Nil}})

		arr = append(arr, &z3.Cell{Car: z3.NewSym("is-mobile?"),
			Cdr: &z3.Cell{Car: z3.AsLispBool(dev.IsMobile()), Cdr: z3.Nil}})
		arr = append(arr, &z3.Cell{Car: z3.NewSym("is-browser?"),
			Cdr: &z3.Cell{Car: z3.AsLispBool(dev.IsBrowser()), Cdr: z3.Nil}})
		arr = append(arr, &z3.Cell{Car: z3.NewSym("has-keyboard?"),
			Cdr: &z3.Cell{Car: z3.AsLispBool(dev.HasKeyboard()), Cdr: z3.Nil}})
		arr = append(arr, &z3.Cell{Car: z3.NewSym("system-scale"),
			Cdr: &z3.Cell{Car: goarith.AsNumber(math.Abs(float64(dev.SystemScaleForWindow(mainWin)))), Cdr: z3.Nil}})

		return z3.ArrayToList(arr)
	})

	// LAYOUTS

	interp.Def(pre("new-spacer"), 0, func(a []any) any {
		return put(layout.NewSpacer())
	})

	interp.Def(pre("new-hbox-layout"), 0, func(a []any) any {
		return put(layout.NewHBoxLayout())
	})

	interp.Def(pre("new-vbox-layout"), 0, func(a []any) any {
		return put(layout.NewVBoxLayout())
	})

	interp.Def(pre("new-grid-layout"), 1, func(a []any) any {
		n := z3.ToInt64(pre("new-grid-layout"), a[0])
		return put(layout.NewGridLayout(int(n)))
	})

	interp.Def(pre("new-grid-wrap-layout"), 2, func(a []any) any {
		w := z3.ToFloat64(a[0])
		h := z3.ToFloat64(a[1])
		return put(layout.NewGridWrapLayout(fyne.NewSize(float32(w), float32(h))))
	})

	interp.Def(pre("new-form-layout"), 0, func(a []any) any {
		return put(layout.NewFormLayout())
	})

	interp.Def(pre("new-center-layout"), 0, func(a []any) any {
		return put(layout.NewCenterLayout())
	})

	interp.Def(pre("new-stack-layout"), 0, func(a []any) any {
		return put(layout.NewStackLayout())
	})

	// CONTAINER

	interp.Def(pre("new-container"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		layout := mustGet1(pre("new-container"), "layout", li.Car)
		li = li.CdrCell()
		objs := make([]fyne.CanvasObject, 0, len(a)-1)
		for li != z3.Nil {
			obj, ok := get(li.Car)
			if !ok {
				panic(fmt.Sprintf(pre("new-container: unknown GUI object ID, given %v"), z3.Str(li.Car)))
			}
			objs = append(objs, obj.(fyne.CanvasObject))
			li = li.CdrCell()
		}
		c := container.New(layout.(fyne.Layout), objs...)
		return put(c)
	})

	interp.Def(pre("new-container-without-layout"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		objs := make([]fyne.CanvasObject, 0, len(a)-1)
		for li != z3.Nil {
			obj, ok := get(li.Car)
			if !ok {
				panic(fmt.Sprintf(pre("new-container: unknown GUI object ID, given %v"), z3.Str(li.Car)))
			}
			objs = append(objs, obj.(fyne.CanvasObject))
			li = li.CdrCell()
		}
		c := container.NewWithoutLayout(objs...)
		return put(c)
	})

	interp.Def(pre("new-border"), -1, func(a []any) any {
		arr := make([]fyne.CanvasObject, 0)
		args := z3.ListToArray(a[0].(*z3.Cell))
		if len(args) < 4 {
			panic(fmt.Sprintf("new-border requires at least top, bottom, left, right arguments, given: %v",
				a[0].(*z3.Cell)))
		}
		for _, arg := range args {
			if cell, ok := arg.(*z3.Cell); ok {
				if cell != z3.Nil {
					panic(fmt.Sprintf(pre("new-border: expected a valid GUI object ID or nil, given a non-nil list: %v"),
						z3.Str(arg)))
				}
				arr = append(arr, nil)
				continue
			}
			obj, ok := get(arg)
			if !ok {
				panic(fmt.Sprintf(pre("new-border: expected a valid GUI object ID or nil, given: %v"),
					z3.Str(arg)))
			}
			if canvas, ok := obj.(fyne.CanvasObject); ok {
				arr = append(arr, canvas)
				continue
			}
			panic(fmt.Sprintf(pre("new-border: expected a valid GUI canvas object, but the given %v is not a canvas object"), z3.Str(arg)))
		}
		return put(container.NewBorder(arr[0], arr[1], arr[2], arr[3], arr[4:]...))
	})

	interp.Def(pre("new-tabitem"), 2, func(a []any) any {
		title := a[0].(string)
		arg := mustGet1(pre("new-tabitem"), "GUI canvas object ID", a[1])
		obj, ok := arg.(fyne.CanvasObject)
		if !ok {
			panic(fmt.Sprintf(pre("new-tabitem argument must be a canvas object, given %v"), z3.Str(arg)))
		}
		return put(container.NewTabItem(title, obj))
	})

	interp.Def(pre("new-tabitem-with-icon"), 3, func(a []any) any {
		title := a[0].(string)
		icon := mustGet(pre("new-tabitem-with-icon"), "GUI icon resource ID", a, 1)
		ics, ok := icon.(fyne.Resource)
		if !ok {
			panic(fmt.Sprintf(pre("new-tabitem-with-icon expected an icon resource as second argument, received: %v"), z3.Str(a[1])))
		}
		canvas := mustGet(pre("new-tabitem-with-icon"), "GUI canvas object ID", a, 2)
		return put(container.NewTabItemWithIcon(title, ics, canvas.(fyne.CanvasObject)))
	})

	// (new-app-tabs tab-item ...) => int
	interp.Def(pre("new-app-tabs"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		arr := make([]*container.TabItem, 0)
		for li != z3.Nil {
			tab := mustGet1(pre("new-app-tabs"), "GUI tabitem ID", li.Car)
			arr = append(arr, tab.(*container.TabItem))
			li = li.CdrCell()
		}
		return put(container.NewAppTabs(arr...))
	})

	// (new-doc-tabs tab-item ...) => int
	interp.Def(pre("new-doc-tabs"), -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		arr := make([]*container.TabItem, 0)
		for li != z3.Nil {
			tab := mustGet1(pre("new-app-tabs"), "GUI tabitem ID", li.Car)
			arr = append(arr, tab.(*container.TabItem))
			li = li.CdrCell()
		}
		return put(container.NewDocTabs(arr...))
	})

	// (new-hsplit leading trailing) => int
	interp.Def(pre("new-hsplit"), 2, func(a []any) any {
		lead := mustGet(pre("new-hsplit"), "GUI canvas object ID", a, 0).(fyne.CanvasObject)
		trail := mustGet(pre("new-hsplit"), "GUI canvas object ID", a, 1).(fyne.CanvasObject)
		return put(container.NewHSplit(lead, trail))
	})

	// (new-vsplit leading trailing) => int
	interp.Def(pre("new-vsplit"), 2, func(a []any) any {
		top := mustGet(pre("new-vsplit"), "GUI canvas object ID", a, 0).(fyne.CanvasObject)
		bottom := mustGet(pre("new-vsplit"), "GUI canvas object ID", a, 1).(fyne.CanvasObject)
		return put(container.NewVSplit(top, bottom))
	})

	// (set-split-offset split fl)
	interp.Def(pre("set-split-offset"), 2, func(a []any) any {
		split := mustGet(pre("set-split-offset"), "GUI split ID", a, 0).(*container.Split)
		fl := z3.ToFloat64(a[1])
		split.SetOffset(fl)
		return z3.Void
	})

	// RESOURCES

	// THEME ICONS
	interp.Def(pre("theme-icon"), 1, func(a []any) any {
		var name string
		if sym, ok := a[0].(*z3.Sym); ok {
			name = sym.String()
		} else {
			name = a[0].(string)
		}
		var res fyne.Resource
		switch name {
		case "account", "AccountIcon":
			res = theme.AccountIcon()
			// The following seem to be misdocumented:
		// case "arrow-drop-down", "ArrowDropDownIcon":
		// 	res = theme.Icon(theme.IconNameArrowDropDown)
		// case "arrow-drop-up", "ArrowDropUpIcon":
		// 	res = theme.ArrowDropUpIcon()
		case "cancel", "CancelIcon":
			res = theme.CancelIcon()
		case "check-button-checked", "CheckButtonChecked":
			res = theme.CheckButtonCheckedIcon()
		case "check-button", "CheckButtonIcon":
			res = theme.CheckButtonIcon()
		case "color-achromatic", "ColorAchromaticIcon":
			res = theme.ColorAchromaticIcon()
		case "color-chromatic", "ColorChromaticIcon":
			res = theme.ColorChromaticIcon()
		case "color-palette", "ColorPaletteIcon":
			res = theme.ColorPaletteIcon()
		case "computer", "ComputerIcon":
			res = theme.ComputerIcon()
		case "confirm", "ConfirmIcon":
			res = theme.ConfirmIcon()
		case "content-add", "ContentAddIcon":
			res = theme.ContentAddIcon()
		case "content-clear", "ContentClearIcon":
			res = theme.ContentClearIcon()
		case "content-copy", "ContentCopyIcon":
			res = theme.ContentCopyIcon()
		case "content-cut", "ContentCutIcon":
			res = theme.ContentCutIcon()
		case "content-paste", "ContentPasteIcon":
			res = theme.ContentPasteIcon()
		case "content-redo", "ContentRedoIcon":
			res = theme.ContentRedoIcon()
		case "content-remove", "ContentRemoveIcon":
			res = theme.ContentRemoveIcon()
		case "content-undo", "ContentUndoIcon":
			res = theme.ContentUndoIcon()
		case "delete", "DeleteIcon":
			res = theme.DeleteIcon()
		case "document-create", "DocumentCreateIcon":
			res = theme.DocumentCreateIcon()
		case "document-print", "DocumentPrintIcon":
			res = theme.DocumentPrintIcon()
		case "document", "DocumentIcon":
			res = theme.DocumentIcon()
		case "download", "DownloadIcon":
			res = theme.DownloadIcon()
		case "error", "ErrorIcon":
			res = theme.ErrorIcon()
		case "file-application", "FileApplicationIcon":
			res = theme.FileApplicationIcon()
		case "file-audio", "FileAudioIcon":
			res = theme.FileAudioIcon()
		case "file-image", "FileImageIcon":
			res = theme.FileImageIcon()
		case "file-text", "FileTextIcon":
			res = theme.FileTextIcon()
		case "file-video", "FileVideoIcon":
			res = theme.FileVideoIcon()
		case "file", "FileIcon":
			res = theme.FileIcon()
		case "folder-new", "FolderNewIcon":
			res = theme.FolderNewIcon()
		case "folder-open", "FolderOpenIcon":
			res = theme.FolderOpenIcon()
		case "folder", "FolderIcon":
			res = theme.FolderIcon()
		case "grid", "GridIcon":
			res = theme.GridIcon()
		case "help", "HelpIcon":
			res = theme.HelpIcon()
		case "history", "HistoryIcon":
			res = theme.HistoryIcon()
		case "home", "HomeIcon":
			res = theme.HomeIcon()
		case "info", "InfoIcon":
			res = theme.InfoIcon()
		case "list", "ListIcon":
			res = theme.ListIcon()
		case "login", "LoginIcon":
			res = theme.LoginIcon()
		case "logout", "LogoutIcon":
			res = theme.LogoutIcon()
		case "mail-attachment", "MailAttachmentIcon":
			res = theme.MailAttachmentIcon()
		case "mail-compose", "MailComposeIcon":
			res = theme.MailComposeIcon()
		case "mail-forward", "MailForwardIcon":
			res = theme.MailForwardIcon()
		case "mail-reply-all", "MailReplyAllIcon":
			res = theme.MailReplyAllIcon()
		case "mail-reply", "MailReplyIcon":
			res = theme.MailReplyIcon()
		case "mail-send", "MailSendIcon":
			res = theme.MailSendIcon()
		case "media-fast-forward", "MediaFastForwardIcon":
			res = theme.MediaFastForwardIcon()
		case "media-fast-rewind", "MediaFastRewindIcon":
			res = theme.MediaFastRewindIcon()
		case "media-music", "MediaMusicIcon":
			res = theme.MediaMusicIcon()
		case "media-pause", "MediaPauseIcon":
			res = theme.MediaPauseIcon()
		case "media-photo", "MediaPhotoIcon":
			res = theme.MediaPhotoIcon()
		case "media-play", "MediaPlayIcon":
			res = theme.MediaPlayIcon()
		case "media-record", "MediaRecordIcon":
			res = theme.MediaRecordIcon()
		case "media-replay", "MediaReplayIcon":
			res = theme.MediaReplayIcon()
		case "media-skip-next", "MediaSkipNextIcon":
			res = theme.MediaSkipNextIcon()
		case "media-skip-previous", "MediaSkipPreviousIcon":
			res = theme.MediaSkipPreviousIcon()
		case "media-stop", "MediaStopIcon":
			res = theme.MediaStopIcon()
		case "media-video", "MediaVideoIcon":
			res = theme.MediaVideoIcon()
		case "menu-expand", "MenuExpandIcon":
			res = theme.MenuExpandIcon()
		case "menu", "MenuIcon":
			res = theme.MenuIcon()
		case "more-horizontal", "MoreHorizontalIcon":
			res = theme.MoreHorizontalIcon()
		case "more-vertical", "MoreVerticalIcon":
			res = theme.MoreVerticalIcon()
		case "move-down", "MoveDownIcon":
			res = theme.MoveDownIcon()
		case "move-up", "MoveUpIcon":
			res = theme.MoveUpIcon()
		case "navigate-back", "NavigateBackIcon":
			res = theme.NavigateBackIcon()
		case "navigate-next", "NavigateNextIcon":
			res = theme.NavigateNextIcon()
		case "question", "QuestionIcon":
			res = theme.QuestionIcon()
		case "radio-button-checked", "RadioButtonChecked":
			res = theme.RadioButtonCheckedIcon()
		case "radio-button", "RadioButtonIcon":
			res = theme.RadioButtonIcon()
		case "search-replace", "SearchReplaceIcon":
			res = theme.SearchReplaceIcon()
		case "search", "SearchIcon":
			res = theme.SearchIcon()
		case "settings", "SettingsIcon":
			res = theme.SettingsIcon()
		case "storage", "StorageIcon":
			res = theme.StorageIcon()
		case "upload", "UploadIcon":
			res = theme.UploadIcon()
		case "view-full-screen", "ViewFullScreenIcon":
			res = theme.ViewFullScreenIcon()
		case "view-refresh", "ViewRefreshIcon":
			res = theme.ViewRefreshIcon()
		case "view-restore", "ViewRestoreIcon":
			res = theme.ViewRestoreIcon()
			// The following seem to be unavailable / misdocumented:
		// case "view-zoom-fit", "ViewZoomFitIcon":
		// 	res = theme.ViewZoomFitIcon()
		// case "view-zoom-in", "ViewZoomInIcon":
		// 	res = theme.ViewZoomInIcon()
		// case "view-zoom-out", "ViewZoomOutIcon":
		// 	res = theme.ViewZoomOutIcon()
		case "visibility-off", "VisibilityOffIcon":
			res = theme.VisibilityOffIcon()
		case "visibility", "VisibilityIcon":
			res = theme.VisibilityIcon()
		case "volume-down", "VolumeDownIcon":
			res = theme.VolumeDownIcon()
		case "volume-mute", "VolumeMuteIcon":
			res = theme.VolumeMuteIcon()
		case "volume-up", "VolumeUpIcon":
			res = theme.VolumeUpIcon()
		case "warning", "WarningIcon":
			res = theme.WarningIcon()
		default:
			panic(fmt.Sprintf(pre("theme-icon: unknown theme icon name, given %v"), z3.Str(a[0])))
		}
		return put(res)
	})

}

// MustGetPosition expects a position in argument a at argument index argIdx and returns it,
// panics if a is not a position list. The argument may be nil, in case of which bool is false.
func MustGetPosition(caller string, argIdx int, a any) (fyne.Position, bool) {
	li, ok := a.(*z3.Cell)
	if !ok {
		panic(fmt.Sprintf("%v: expected GUI position list as %v argument, given %v", caller,
			idxToEnglish(argIdx), z3.Str(a)))
	}
	if li == z3.Nil {
		return fyne.Position{}, false
	}
	x := float32(z3.ToFloat64(li.Car))
	li = li.CdrCell()
	y := float32(z3.ToFloat64(li.Car))
	return fyne.Position{X: x, Y: y}, true
}

func idxToEnglish(idx int) string {
	switch idx {
	case 0:
		return "first"
	case 1:
		return "second"
	case 2:
		return "third"
	case 3:
		return "fourth"
	case 4:
		return "fifth"
	case 5:
		return "sixth"
	case 6:
		return "seventh"
	default:
		return fmt.Sprintf("%vth", idx+1)
	}
}

// MustGetTextAlign returns a text align and true, or nil and false if the alignment specified was Nil.
func MustGetTextAlign(caller string, idx int, a any) (fyne.TextAlign, bool) {
	sym, ok := a.(*z3.Sym)
	if !ok {
		if li, ok := a.(*z3.Cell); ok {
			if li == z3.Nil {
				return fyne.TextAlignLeading, false
			}
			panic(fmt.Sprintf("%v: expected text alignment symbol in '(leading center trailing) as %v argument, given a non-empty list", caller, idxToEnglish(idx)))
		}
		panic(fmt.Sprintf("%v: expected text alignment symbol in '(leading center trailing) as %v argument, given %v", caller, idxToEnglish(idx), z3.Str(a)))
	}
	switch sym.String() {
	case "leading":
		return fyne.TextAlignLeading, true
	case "center":
		return fyne.TextAlignCenter, true
	case "trailing":
		return fyne.TextAlignTrailing, true
	default:
		panic(fmt.Sprintf("%v: expected text alignment symbol in '(leading center trailing) as %v argument, given %v", caller, idxToEnglish(idx), z3.Str(a)))
	}
}

// MustGetTextStyle turns a list of style symbols into a Fyne text style.
func MustGetTextStyle(caller string, idx int, a any) fyne.TextStyle {
	li := a.(*z3.Cell)
	var style fyne.TextStyle
	for li != z3.Nil {
		sym, ok := li.Car.(*z3.Sym)
		if !ok {
			panic(fmt.Sprintf("%v: expected a list of valid text style symbols as %v argument, given %v", caller, idxToEnglish(idx),
				z3.Str(a)))
		}
		switch sym.String() {
		case "bold":
			style.Bold = true
		case "italic":
			style.Italic = true
		case "mono", "monospace":
			style.Monospace = true
		case "symbol":
			style.Symbol = true
		case "tab-width":
			li = li.CdrCell()
			n, _ := goarith.AsNumber(li.Car).Int()
			style.TabWidth = n
		}
		li = li.CdrCell()
	}
	return style
}

// KeyNameToSymbol converts a Fyne key name string to a symbol, as they are used in MustGetShortcut.
func KeyNameToSymbol(name fyne.KeyName) *z3.Sym {
	switch name {
	case fyne.KeyEscape:
		return KeyEscape
	case fyne.KeyReturn:
		return KeyReturn
	case fyne.KeyTab:
		return KeyTab
	case fyne.KeyBackspace:
		return KeyBackspace
	case fyne.KeyInsert:
		return KeyInsert
	case fyne.KeyDelete:
		return KeyDelete
	case fyne.KeyRight:
		return KeyRight
	case fyne.KeyLeft:
		return KeyLeft
	case fyne.KeyDown:
		return KeyDown
	case fyne.KeyUp:
		return KeyUp
	case fyne.KeyPageDown:
		return KeyPageDown
	case fyne.KeyPageUp:
		return KeyPageUp
	case fyne.KeyHome:
		return KeyHome
	case fyne.KeyEnd:
		return KeyEnd
	case fyne.KeyF1:
		return KeyF1
	case fyne.KeyF2:
		return KeyF2
	case fyne.KeyF3:
		return KeyF3
	case fyne.KeyF4:
		return KeyF4
	case fyne.KeyF5:
		return KeyF5
	case fyne.KeyF6:
		return KeyF6
	case fyne.KeyF7:
		return KeyF7
	case fyne.KeyF8:
		return KeyF8
	case fyne.KeyF9:
		return KeyF9
	case fyne.KeyF10:
		return KeyF10
	case fyne.KeyF11:
		return KeyF11
	case fyne.KeyEnter:
		return KeyEnter
	case fyne.Key0:
		return Key0
	case fyne.Key1:
		return Key1
	case fyne.Key2:
		return Key2
	case fyne.Key3:
		return Key3
	case fyne.Key4:
		return Key4
	case fyne.Key5:
		return Key5
	case fyne.Key6:
		return Key6
	case fyne.Key7:
		return Key7
	case fyne.Key8:
		return Key8
	case fyne.Key9:
		return Key9
	case fyne.KeyA:
		return KeyA
	case fyne.KeyB:
		return KeyB
	case fyne.KeyC:
		return KeyC
	case fyne.KeyD:
		return KeyD
	case fyne.KeyE:
		return KeyE
	case fyne.KeyF:
		return KeyF
	case fyne.KeyG:
		return KeyG
	case fyne.KeyH:
		return KeyH
	case fyne.KeyI:
		return KeyI
	case fyne.KeyJ:
		return KeyJ
	case fyne.KeyK:
		return KeyK
	case fyne.KeyL:
		return KeyL
	case fyne.KeyM:
		return KeyM
	case fyne.KeyN:
		return KeyN
	case fyne.KeyO:
		return KeyO
	case fyne.KeyP:
		return KeyP
	case fyne.KeyQ:
		return KeyQ
	case fyne.KeyR:
		return KeyR
	case fyne.KeyS:
		return KeyS
	case fyne.KeyT:
		return KeyT
	case fyne.KeyU:
		return KeyU
	case fyne.KeyV:
		return KeyV
	case fyne.KeyW:
		return KeyW
	case fyne.KeyX:
		return KeyX
	case fyne.KeyY:
		return KeyY
	case fyne.KeyZ:
		return KeyZ
	case fyne.KeySpace:
		return KeySpace
	case fyne.KeyApostrophe:
		return KeyTick
	case fyne.KeyComma:
		return KeyComma
	case fyne.KeyMinus:
		return KeyMinus
	case fyne.KeyPeriod:
		return KeyPeriod
	case fyne.KeySlash:
		return KeySlash
	case fyne.KeyBackslash:
		return KeyBackslash
	case fyne.KeyLeftBracket:
		return KeyLeftBracket
	case fyne.KeyRightBracket:
		return KeyRightBracket
	case fyne.KeySemicolon:
		return KeySemicolon
	case fyne.KeyEqual:
		return KeyEqual
	case fyne.KeyAsterisk:
		return KeyAsterisk
	case fyne.KeyPlus:
		return KeyPlus
	case fyne.KeyBackTick:
		return KeyBackTick
	default:
		return KeyUnknown
	}
}

// MustGetShortcut converts a Z3S5 Lisp list shortcut representations into the key name and modifier
// of a fyne.KeyShortcut.
func MustGetShortcut(caller string, li *z3.Cell) (fyne.KeyName, fyne.KeyModifier) {
	var mod fyne.KeyModifier
	var key fyne.KeyName
	var s string
	for li != z3.Nil {
		sym, ok := li.Car.(*z3.Sym)
		if ok {
			switch sym {
			case KeyShift:
				mod = mod | fyne.KeyModifierShift
				li = li.CdrCell()
				continue
			case KeyControl:
				mod = mod | fyne.KeyModifierControl
				li = li.CdrCell()
				continue
			case KeyAlt:
				mod = mod | fyne.KeyModifierAlt
				li = li.CdrCell()
				continue
			case KeySuper:
				mod = mod | fyne.KeyModifierSuper
				li = li.CdrCell()
				continue
			case KeyEscape:
				key = fyne.KeyEscape
				li = li.CdrCell()
				continue
			case KeyReturn:
				key = fyne.KeyReturn
				li = li.CdrCell()
				continue
			case KeyTab:
				key = fyne.KeyTab
				li = li.CdrCell()
				continue
			case KeyBackspace:
				key = fyne.KeyBackspace
				li = li.CdrCell()
				continue
			case KeyInsert:
				key = fyne.KeyInsert
				li = li.CdrCell()
				continue
			case KeyDelete:
				key = fyne.KeyDelete
				li = li.CdrCell()
				continue
			case KeyRight:
				key = fyne.KeyRight
				li = li.CdrCell()
				continue
			case KeyLeft:
				key = fyne.KeyLeft
				li = li.CdrCell()
				continue
			case KeyDown:
				key = fyne.KeyDown
				li = li.CdrCell()
				continue
			case KeyUp:
				key = fyne.KeyUp
				li = li.CdrCell()
				continue
			case KeyPageUp:
				key = fyne.KeyPageUp
				li = li.CdrCell()
				continue
			case KeyPageDown:
				key = fyne.KeyPageDown
				li = li.CdrCell()
				continue
			case KeyHome:
				key = fyne.KeyHome
				li = li.CdrCell()
				continue
			case KeyEnd:
				key = fyne.KeyEnd
				li = li.CdrCell()
				continue
			case KeyF1:
				key = fyne.KeyF1
				li = li.CdrCell()
				continue
			case KeyF2:
				key = fyne.KeyF2
				li = li.CdrCell()
				continue
			case KeyF3:
				key = fyne.KeyF3
				li = li.CdrCell()
				continue
			case KeyF4:
				key = fyne.KeyF4
				li = li.CdrCell()
				continue
			case KeyF5:
				key = fyne.KeyF5
				li = li.CdrCell()
				continue
			case KeyF6:
				key = fyne.KeyF6
				li = li.CdrCell()
				continue
			case KeyF7:
				key = fyne.KeyF7
				li = li.CdrCell()
				continue
			case KeyF8:
				key = fyne.KeyF8
				li = li.CdrCell()
				continue
			case KeyF9:
				key = fyne.KeyF9
				li = li.CdrCell()
				continue
			case KeyF10:
				key = fyne.KeyF10
				li = li.CdrCell()
				continue
			case KeyF11:
				key = fyne.KeyF11
				li = li.CdrCell()
				continue
			case KeyF12:
				key = fyne.KeyF12
				li = li.CdrCell()
				continue
			case KeyEnter:
				key = fyne.KeyEnter
				li = li.CdrCell()
				continue
			case Key0:
				key = fyne.Key0
				li = li.CdrCell()
				continue
			case Key1:
				key = fyne.Key1
				li = li.CdrCell()
				continue
			case Key2:
				key = fyne.Key2
				li = li.CdrCell()
				continue
			case Key3:
				key = fyne.Key3
				li = li.CdrCell()
				continue
			case Key4:
				key = fyne.Key4
				li = li.CdrCell()
				continue
			case Key5:
				key = fyne.Key5
				li = li.CdrCell()
				continue
			case Key6:
				key = fyne.Key6
				li = li.CdrCell()
				continue
			case Key7:
				key = fyne.Key7
				li = li.CdrCell()
				continue
			case Key8:
				key = fyne.Key8
				li = li.CdrCell()
				continue
			case Key9:
				key = fyne.Key9
				li = li.CdrCell()
				continue
			case KeyA:
				key = fyne.KeyA
				li = li.CdrCell()
				continue
			case KeyB:
				key = fyne.KeyB
				li = li.CdrCell()
				continue
			case KeyC:
				key = fyne.KeyC
				li = li.CdrCell()
				continue
			case KeyD:
				key = fyne.KeyD
				li = li.CdrCell()
				continue
			case KeyE:
				key = fyne.KeyE
				li = li.CdrCell()
				continue
			case KeyF:
				key = fyne.KeyF
				li = li.CdrCell()
				continue
			case KeyG:
				key = fyne.KeyG
				li = li.CdrCell()
				continue
			case KeyH:
				key = fyne.KeyH
				li = li.CdrCell()
				continue
			case KeyI:
				key = fyne.KeyI
				li = li.CdrCell()
				continue
			case KeyJ:
				key = fyne.KeyJ
				li = li.CdrCell()
				continue
			case KeyK:
				key = fyne.KeyK
				li = li.CdrCell()
				continue
			case KeyL:
				key = fyne.KeyL
				li = li.CdrCell()
				continue
			case KeyM:
				key = fyne.KeyM
				li = li.CdrCell()
				continue
			case KeyN:
				key = fyne.KeyN
				li = li.CdrCell()
				continue
			case KeyO:
				key = fyne.KeyO
				li = li.CdrCell()
				continue
			case KeyP:
				key = fyne.KeyP
				li = li.CdrCell()
				continue
			case KeyQ:
				key = fyne.KeyQ
				li = li.CdrCell()
				continue
			case KeyR:
				key = fyne.KeyR
				li = li.CdrCell()
				continue
			case KeyS:
				key = fyne.KeyS
				li = li.CdrCell()
				continue
			case KeyT:
				key = fyne.KeyT
				li = li.CdrCell()
				continue
			case KeyU:
				key = fyne.KeyU
				li = li.CdrCell()
				continue
			case KeyV:
				key = fyne.KeyV
				li = li.CdrCell()
				continue
			case KeyW:
				key = fyne.KeyW
				li = li.CdrCell()
				continue
			case KeyX:
				key = fyne.KeyX
				li = li.CdrCell()
				continue
			case KeyY:
				key = fyne.KeyY
				li = li.CdrCell()
				continue
			case KeyZ:
				key = fyne.KeyZ
				li = li.CdrCell()
				continue
			case KeySpace:
				key = fyne.KeySpace
				li = li.CdrCell()
				continue
			case KeyTick:
				key = fyne.KeyApostrophe
				li = li.CdrCell()
				continue
			case KeyComma:
				key = fyne.KeyComma
				li = li.CdrCell()
				continue
			case KeyMinus:
				key = fyne.KeyMinus
				li = li.CdrCell()
				continue
			case KeyPeriod:
				key = fyne.KeyPeriod
				li = li.CdrCell()
				continue
			case KeySlash:
				key = fyne.KeySlash
				li = li.CdrCell()
				continue
			case KeyBackslash:
				key = fyne.KeyBackslash
				li = li.CdrCell()
				continue
			case KeyLeftBracket:
				key = fyne.KeyLeftBracket
				li = li.CdrCell()
				continue
			case KeyRightBracket:
				key = fyne.KeyRightBracket
				li = li.CdrCell()
				continue
			case KeySemicolon:
				key = fyne.KeySemicolon
				li = li.CdrCell()
				continue
			case KeyEqual:
				key = fyne.KeyEqual
				li = li.CdrCell()
				continue
			case KeyAsterisk:
				key = fyne.KeyAsterisk
				li = li.CdrCell()
				continue
			case KeyPlus:
				key = fyne.KeyPlus
				li = li.CdrCell()
				continue
			case KeyBackTick:
				key = fyne.KeyBackTick
				li = li.CdrCell()
				continue
			}
			s = sym.String()
		} else if str, ok := li.Car.(string); ok {
			s = str
		} else if n, ok := li.Car.(goarith.Number); ok {
			s = fmt.Sprintf("%v", n)
		} else {
			panic(fmt.Sprintf("%v: expected valid keyboard shortcut list, given %v", caller, z3.Str(li)))
		}
		switch s {
		case "shift":
			mod = mod | fyne.KeyModifierShift
		case "control", "ctrl":
			mod = mod | fyne.KeyModifierControl
		case "alt":
			mod = mod | fyne.KeyModifierAlt
		case "super":
			mod = mod | fyne.KeyModifierSuper
		case "esc", "escape":
			key = fyne.KeyEscape
		case "ret", "return":
			key = fyne.KeyReturn
		case "tab":
			key = fyne.KeyTab
		case "backspace", "bkspc":
			key = fyne.KeyBackspace
		case "insert", "ins":
			key = fyne.KeyInsert
		case "del", "delete":
			key = fyne.KeyDelete
		case "right":
			key = fyne.KeyRight
		case "left":
			key = fyne.KeyLeft
		case "down":
			key = fyne.KeyDown
		case "up":
			key = fyne.KeyUp
		case "page-up":
			key = fyne.KeyPageUp
		case "page-down":
			key = fyne.KeyPageDown
		case "home":
			key = fyne.KeyHome
		case "end":
			key = fyne.KeyEnd
		case "f1":
			key = fyne.KeyF1
		case "f2":
			key = fyne.KeyF2
		case "f3":
			key = fyne.KeyF3
		case "f4":
			key = fyne.KeyF4
		case "f5":
			key = fyne.KeyF5
		case "f6":
			key = fyne.KeyF6
		case "f7":
			key = fyne.KeyF7
		case "f8":
			key = fyne.KeyF8
		case "f9":
			key = fyne.KeyF9
		case "f10":
			key = fyne.KeyF10
		case "f11":
			key = fyne.KeyF11
		case "f12":
			key = fyne.KeyF12
		case "enter":
			key = fyne.KeyEnter
		case "key0", "0":
			key = fyne.Key0
		case "key1", "1":
			key = fyne.Key1
		case "key2", "2":
			key = fyne.Key2
		case "key3", "3":
			key = fyne.Key3
		case "key4", "4":
			key = fyne.Key4
		case "key5", "5":
			key = fyne.Key5
		case "key6", "6":
			key = fyne.Key6
		case "key7", "7":
			key = fyne.Key7
		case "key8", "8":
			key = fyne.Key8
		case "key9", "9":
			key = fyne.Key9
		case "a":
			key = fyne.KeyA
		case "b":
			key = fyne.KeyB
		case "c":
			key = fyne.KeyC
		case "d":
			key = fyne.KeyD
		case "e":
			key = fyne.KeyE
		case "f":
			key = fyne.KeyF
		case "g":
			key = fyne.KeyG
		case "h":
			key = fyne.KeyH
		case "i":
			key = fyne.KeyI
		case "j":
			key = fyne.KeyJ
		case "k":
			key = fyne.KeyK
		case "l":
			key = fyne.KeyL
		case "m":
			key = fyne.KeyM
		case "n":
			key = fyne.KeyN
		case "o":
			key = fyne.KeyO
		case "p":
			key = fyne.KeyP
		case "q":
			key = fyne.KeyQ
		case "r":
			key = fyne.KeyR
		case "s":
			key = fyne.KeyS
		case "t":
			key = fyne.KeyT
		case "u":
			key = fyne.KeyU
		case "v":
			key = fyne.KeyV
		case "w":
			key = fyne.KeyW
		case "x":
			key = fyne.KeyX
		case "y":
			key = fyne.KeyY
		case "z":
			key = fyne.KeyZ
		case "space", "spc", " ":
			key = fyne.KeySpace
		case "apostrophe", "tick", "'":
			key = fyne.KeyApostrophe
		case "comma", ",":
			key = fyne.KeyComma
		case "minus", "-":
			key = fyne.KeyMinus
		case "period", "dot", ".":
			key = fyne.KeyPeriod
		case "slash", "/":
			key = fyne.KeySlash
		case "backslash", "\\":
			key = fyne.KeyBackslash
		case "left-bracket", "lbracket", "[":
			key = fyne.KeyLeftBracket
		case "right-bracket", "rbracket", "]":
			key = fyne.KeyRightBracket
		case "semicolon", ";":
			key = fyne.KeySemicolon
		case "equal", "=":
			key = fyne.KeyEqual
		case "asterisk", "*":
			key = fyne.KeyAsterisk
		case "plus", "+":
			key = fyne.KeyPlus
		case "back-tick", "backtick", "`":
			key = fyne.KeyBackTick
		case "unknown", "":
			key = fyne.KeyUnknown
		default:
			panic(fmt.Sprintf("%v: expected list of valid keyboard shortcut symbols, strings, or digits, but '%v is not valid; other arguments were '%v", caller, s, z3.Str(li)))
		}
		li = li.CdrCell()
	}
	return key, mod
}

var DefaultCursorSym = z3.NewSym("default")
var TextCursorSym = z3.NewSym("text")
var CrosshairCursorSym = z3.NewSym("crosshair")
var PointerCursorSym = z3.NewSym("pointer")
var HResizeCursorSym = z3.NewSym("hresize")
var VResizeCursorSym = z3.NewSym("vresize")

// CursorToSym converts a Fyne desktop.Cursor to a symbol.
func CursorToSym(c desktop.Cursor) *z3.Sym {
	switch c {
	case desktop.DefaultCursor:
		return DefaultCursorSym
	case desktop.TextCursor:
		return TextCursorSym
	case desktop.CrosshairCursor:
		return CrosshairCursorSym
	case desktop.PointerCursor:
		return PointerCursorSym
	case desktop.HResizeCursor:
		return HResizeCursorSym
	case desktop.VResizeCursor:
		return VResizeCursorSym
	default:
		panic(fmt.Sprintf("unknown cursor value: %v", c))
	}
}

// ColorToList converts a Go color.Color to a Z3S5 Lisp list of numbers.
func ColorToList(c color.Color) *z3.Cell {
	r, g, b, alpha := c.RGBA()
	return &z3.Cell{Car: goarith.AsNumber(int(r)), Cdr: &z3.Cell{Car: goarith.AsNumber(int(g)),
		Cdr: &z3.Cell{Car: goarith.AsNumber(int(b)), Cdr: &z3.Cell{Car: goarith.AsNumber(int(alpha)),
			Cdr: z3.Nil}}}}
}

// ListToColor converts a Z3S5 Lisp color list to a color.
func ListToColor(li *z3.Cell) color.Color {
	r := z3.ToUInt8(li.Car)
	li = li.CdrCell()
	g := z3.ToUInt8(li.Car)
	li = li.CdrCell()
	b := z3.ToUInt8(li.Car)
	li = li.CdrCell()
	var alpha uint8
	if li != z3.Nil {
		alpha = z3.ToUInt8(li.Car)
	} else {
		alpha = 255
	}
	return color.NRGBA{R: r, G: g, B: b, A: alpha}
}

// TextGridStyleToList converts a text grid style to a Z3S5 Lisp list.
func TextGridStyleToList(s widget.TextGridStyle) *z3.Cell {
	return &z3.Cell{Car: ColorToList(s.TextColor()), Cdr: &z3.Cell{Car: ColorToList(s.BackgroundColor()),
		Cdr: z3.Nil}}
}

// ListToTextGridStyle converts a list in the format returned by TextGridStyleToList back to
// a Fyne text grid style.
func ListToTextGridStyle(li *z3.Cell) widget.TextGridStyle {
	fg := ListToColor(li.Car.(*z3.Cell))
	li = li.CdrCell()
	bg := ListToColor(li.Car.(*z3.Cell))
	return &widget.CustomTextGridStyle{FGColor: fg, BGColor: bg}
}

// MustConvertSymToTextWrap converts a symbol to a fyne.TextWrap or panics with an error message.
func MustConvertSymToTextWrap(caller string, sym *z3.Sym) fyne.TextWrap {
	switch sym {
	case WrapOffSym:
		return fyne.TextWrapOff
	case WrapBreakSym:
		return fyne.TextWrapBreak
	case WrapWordSym:
		return fyne.TextWrapWord
	default:
		panic(fmt.Sprintf("%v: expected valid text wrap symbol in '(none break word), given: %v", caller, z3.Str(sym)))
	}
}
