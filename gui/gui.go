package ui

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"errors"
	"fmt"
	"image/color"
	"log"
	"math"
	"net/url"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"
	"unicode"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/canvas"
	"fyne.io/fyne/v2/container"
	"fyne.io/fyne/v2/data/validation"
	"fyne.io/fyne/v2/dialog"
	"fyne.io/fyne/v2/driver/desktop"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
	lorem "github.com/drhodes/golorem"
	"github.com/nukata/goarith"
	z3 "github.com/rasteric/z3s5-lisp"
	zedit "github.com/rasteric/zedit-fyne"
	"golang.org/x/exp/slices"
)

//go:embed embed/gui-help.lisp
var helpGUIFile []byte

//go:embed embed/gui.lisp
var embeddedGUIFile []byte

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

// editor events
var EditorCaretMove = z3.NewSym("caret-move")
var EditorWordChange = z3.NewSym("word-change")
var EditorSelectWord = z3.NewSym("select-word")
var EditorUnknown = z3.NewSym("unknown")

// theme selectors
var ForegroundColor = z3.NewSym("foreground")
var BackgroundColor = z3.NewSym("background")
var ButtonColor = z3.NewSym("button")
var DisabledButtonColor = z3.NewSym("disabled-button")
var DisabledColor = z3.NewSym("disabled")
var DisabledTextColor = z3.NewSym("disabled-text")
var ErrorColor = z3.NewSym("error")
var FocusColor = z3.NewSym("focus")
var HoverColor = z3.NewSym("hover")
var InputBackgroundColor = z3.NewSym("input-background")
var InputBorderColor = z3.NewSym("input-border")
var MenuBackgroundColor = z3.NewSym("menu-background")
var OverlayBackgroundColor = z3.NewSym("overlay-background")
var PlaceHolderColor = z3.NewSym("place-holder")
var PressedColor = z3.NewSym("pressed")
var PrimaryColor = z3.NewSym("primary")
var ScrollBarColor = z3.NewSym("scroll-bar")
var SelectionColor = z3.NewSym("selection")
var SeparatorColor = z3.NewSym("separator")
var ShadowColor = z3.NewSym("shadow")
var SuccessColor = z3.NewSym("success")
var WarningColor = z3.NewSym("warning")
var TextGridForegroundColor = z3.NewSym("text-grid-foreground")
var TextGridBackgroundColor = z3.NewSym("text-grid-background")

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

// lorem ipsum selectors for create-lorem-ipsum helper
var LoremWord = z3.NewSym("word")
var LoremSentence = z3.NewSym("sentence")
var LoremParagraph = z3.NewSym("paragraph")

// zedit caret movement selectors
var CaretDown = z3.NewSym("down")
var CaretUp = z3.NewSym("up")
var CaretLeft = z3.NewSym("left")
var CaretRight = z3.NewSym("right")
var CaretHome = z3.NewSym("home")
var CaretEnd = z3.NewSym("end")
var CaretLineStart = z3.NewSym("line-start")
var CaretLineEnd = z3.NewSym("line-end")
var CaretHalfPageDown = z3.NewSym("half-page-down")
var CaretHalfPageUp = z3.NewSym("half-page-up")
var CaretPageDown = z3.NewSym("page-down")
var CaretPageUp = z3.NewSym("page-up")

// zedit tag events
var TagEvtCaretEnter = z3.NewSym("caret-enter")
var TagEvtCaretLeave = z3.NewSym("caret-leave")
var TagEvtUnknown = z3.NewSym("unknown")

// zedit properties
var ZeditShowlineNumbers = z3.NewSym("show-line-numbers?")
var ZeditShowWhitespace = z3.NewSym("show-whitespace?")
var ZeditLineWrap = z3.NewSym("line-wrap?")
var ZeditSoftWrap = z3.NewSym("soft-wrap?")
var ZeditDrawCaret = z3.NewSym("draw-caret?")
var ZeditHighlightParens = z3.NewSym("highlight-parens?")
var ZeditHighlightParenRange = z3.NewSym("highlight-paren-range?")
var ZeditParagraphLineNumbers = z3.NewSym("paragraph-line-numbers?")

var ZeditSelectionTag = z3.NewSym("selection-tag")
var ZeditSelectionStyle = z3.NewSym("selection-style")
var ZeditHighlightTag = z3.NewSym("highlight-tag")
var ZeditHighlightStyle = z3.NewSym("highlight-style")
var ZeditMarkTag = z3.NewSym("mark-tag")
var ZeditMarkTags = z3.NewSym("mark-tags")
var ZeditMarkStyle = z3.NewSym("mark-style")
var ZeditErrorTag = z3.NewSym("error-tag")
var ZeditErrorStyle = z3.NewSym("error-style")
var ZeditParenErrorTag = z3.NewSym("paren-error-tag")
var ZeditBlendFG = z3.NewSym("blend-fg")
var ZeditBlendFGSwitched = z3.NewSym("blend-fg-switched?")
var ZeditBlendBG = z3.NewSym("blend-bg")
var ZeditBlendBGSwitched = z3.NewSym("blend-bg-switched?")
var ZeditSoftLF = z3.NewSym("soft-lf")
var ZeditHardLF = z3.NewSym("hard-lf")
var ZeditScrollFactor = z3.NewSym("scroll-factor")
var ZeditTabWidth = z3.NewSym("tab-width")
var ZeditMinRefreshInterval = z3.NewSym("min-refresh-interval")
var ZeditCharDrift = z3.NewSym("char-drift")
var ZeditCaretBlinkDelay = z3.NewSym("caret-blink-delay")
var ZeditCaretOnDuration = z3.NewSym("caret-on-duration")
var ZeditCaretOffDuration = z3.NewSym("caret-off-duration")
var ZeditTagPreWrite = z3.NewSym("tag-pre-write")
var ZeditTagPostRead = z3.NewSym("tag-post-read")
var ZeditCustomLoader = z3.NewSym("custom-loader")
var ZeditCustomSave = z3.NewSym("custom-save")
var ZeditMaxLines = z3.NewSym("max-lines")
var ZeditMaxColumns = z3.NewSym("max-columns")
var ZeditMaxTags = z3.NewSym("max-tags")
var ZeditMaxPrintLines = z3.NewSym("max-print-lines")
var ZeditTagPreWriteIntrinsic = z3.NewSym("*gui-tag-pre-write*")
var ZeditTagPostReadIntrinsic = z3.NewSym("*gui-tag-post-read*")
var ZeditCustomSaveIntrinsic = z3.NewSym("*gui-zedit-custom-save*")
var ZeditCustomLoadIntrinsic = z3.NewSym("*gui-zedit-custom-load*")
var ZeditGetWordAtLeft = z3.NewSym("get-word-at-left?")
var ZeditLiberalGetWordAt = z3.NewSym("liberal-get-word-at?")
var ZeditStyleBold = z3.NewSym("bold")
var ZeditStyleItalic = z3.NewSym("italic")
var ZeditStyleMonospace = z3.NewSym("monospace")
var ZeditStyleTextColor = z3.NewSym("text-color")
var ZeditStyleBackgroundColor = z3.NewSym("background-color")

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

// newID returns a new, unallocated object ID for use with putWithID and its
// Z3S5 Lisp counterpart.
func newID() (uint64, any) {
	n := atomic.AddUint64(&counter, 1)
	return n, goarith.AsNumber(int64(n))
}

// putWithID adds an object based on a given ID which must have been obtained by newID.
func putWithID(n uint64, obj any) {
	revstore.Store(obj, n)
	storage.Store(n, obj)
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

// getIDOrPut is a combination of getID and put, it gets an objects'd ID if it is stored, puts the object
// and returns the new ID otherwise.
func getIDOrPut(obj any) any {
	n, ok := revstore.Load(obj)
	if !ok {
		return put(obj)
	}
	return goarith.AsNumber(int64(n.(uint64)))
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
// is quit, e.g. via ShutDownUI. Since it blocks, the Lisp interpreter must be started in parallel. The onStarted
// function is called when the application has been started. This can be used for synchronization, for example.
// The provided ID should be unique if the executable is duplicated.
func RunGUI(id string, onStarted func()) {
	apl = app.NewWithID(id)
	apl.Lifecycle().SetOnStarted(onStarted)
	mainWin = apl.NewWindow("Application")
	mainWin.SetMaster()
	mainWin.CenterOnScreen()
	apl.Lifecycle().SetOnEnteredForeground(func() {
		go func() {
			if mainWin != nil {
				mainWin.Hide()
			}
		}()
	})
	mainWin.ShowAndRun()
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

// DefGUIAdditions defines additional functions from an embedded Lisp source.
func DefGUIAdditions(interp *z3.Interp) error {
	help := bytes.NewReader(embeddedGUIFile)
	if !interp.Run(help, z3.NewInternalSource("embed/gui.lisp", "")) {
		return errors.New(`Z3S5 Lisp GUI embedded definitions failed`)
	}
	return nil
}

// defGUINoFileIO defines the part of the GUI that does not involve or require file IO and stream ports.
func defGUINoFileIO(interp *z3.Interp, config Config) {

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

	// APP

	fnSetAppMetadata := pre("set-app-metadata")
	// (set-app-metadata id name version build icon release? custom)
	interp.Def(fnSetAppMetadata, 7, func(a []any) any {
		icon := mustGet(fnSetAppMetadata, "GUI icon resource ID", a, 4).(fyne.Resource)
		custom := a[6].(*z3.Dict)
		m := make(map[string]string)
		custom.Data.Range(func(k, v any) bool {
			m[k.(string)] = v.(string)
			return true
		})
		build := z3.ToInt(fnSetAppMetadata, a[3])
		app.SetMetadata(fyne.AppMetadata{ID: a[0].(string), Name: a[1].(string), Version: a[2].(string),
			Build: build, Icon: icon, Release: z3.ToBool(a[5]), Custom: m})
		return z3.Void
	})

	// WINDOW

	fnNewWindow := pre("new-window")
	// (new-window <title-string>) => window ID
	interp.Def(fnNewWindow, 1, func(a []any) any {
		if !cfg.WindowsAllowed {
			panic(fnNewWindow + ": creating new windows is not permitted!")
		}
		return put(apl.NewWindow(a[0].(string)))
	})

	fnSetWindowContent := pre("set-window-content")
	// (set-window-content <window> <canvas-object>)
	interp.Def(fnSetWindowContent, 2, func(a []any) any {
		win, ok := get(a[0])
		if !ok {
			panic(fmt.Sprintf(fnSetWindowContent+": no window found for %v", z3.Str(a[0])))
		}
		canvas, ok := get(a[1])
		if !ok {
			panic(fmt.Sprintf(fnSetWindowContent+": no canvas object found for %v", z3.Str(a[1])))
		}
		win.(fyne.Window).SetContent(canvas.(fyne.CanvasObject))
		return z3.Void
	})

	fnGetWindowContent := pre("get-window-content")
	// (get-window-content <win>) => content ID
	interp.Def(fnGetWindowContent, 1, func(a []any) any {
		win := mustGet(fnGetWindowContent, "GUI window ID", a, 0).(fyne.Window)
		content := win.Content()
		id, ok := getID(content)
		if !ok {
			return put(content)
		}
		return id
	})

	fnSetWindowSize := pre("set-window-size")
	// (set-window-size <window> <width> <height>)
	interp.Def(fnSetWindowSize, 3, func(a []any) any {
		win := mustGet(fnSetWindowSize, "GUI window ID", a, 0).(fyne.Window)
		win.Resize(fyne.NewSize(float32(z3.ToFloat64(a[1])), float32(z3.ToFloat64(a[2]))))
		return z3.Void
	})

	fnCloseWindow := pre("close-window")
	// (close-window <window>)
	interp.Def(fnCloseWindow, 1, func(a []any) any {
		win := mustGet(fnCloseWindow, "GUI window ID", a, 0)
		win.(fyne.Window).Close()
		clear(win)
		return z3.Void
	})

	fnShowWindow := pre("show-window")
	// (show-window <window>)
	interp.Def(fnShowWindow, 1, func(a []any) any {
		win := mustGet(fnShowWindow, "GUI window ID", a, 0)
		win.(fyne.Window).Show()
		return z3.Void
	})

	fnHideWindow := pre("hide-window")
	// (hide-window <window>)
	interp.Def(fnHideWindow, 1, func(a []any) any {
		win := mustGet(fnHideWindow, "GUI window ID", a, 0)
		win.(fyne.Window).Hide()
		return z3.Void
	})

	fnSetWindowOnCloseCallback := pre("set-window-on-close-callback")
	// (set-window-on-close-callback <window> <callback>)
	interp.Def(fnSetWindowOnCloseCallback, 2, func(a []any) any {
		win := mustGet(fnSetWindowOnCloseCallback, "GUI window ID", a, 0)
		proc := a[1].(*z3.Closure)
		win.(fyne.Window).SetOnClosed(func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.SafeEvalWithInfo(li, z3.Nil,
				"%v\n"+fmt.Sprintf("IN window %v on-close-callback", z3.Str(a[0])))
		})
		return z3.Void
	})

	fnGetWindowCanvas := pre("get-window-canvas")
	// (get-window-canvas <win>) => canvas ID
	interp.Def(fnGetWindowCanvas, 1, func(a []any) any {
		win := mustGet(fnGetWindowCanvas, "GUI window ID", a, 0).(fyne.Window)
		canvas := win.Canvas()
		id, ok := getID(canvas)
		if ok {
			return id
		}
		return put(canvas)
	})

	fnGetWindowTitle := pre("get-window-title")
	// (get-window-title <win>) => str
	interp.Def(fnGetWindowTitle, 1, func(a []any) any {
		win := mustGet(fnGetWindowTitle, "GUI window ID", a, 0).(fyne.Window)
		return win.Title()
	})

	fnSetWindowTitle := pre("set-window-title")
	// (set-window-title <win> <str>)
	interp.Def(fnSetWindowTitle, 2, func(a []any) any {
		win := mustGet(fnSetWindowTitle, "GUI window ID", a, 0).(fyne.Window)
		win.SetTitle(a[1].(string))
		return z3.Void
	})

	fnSetWindowFullScreen := pre("set-window-full-screen")
	// (set-window-full-screen <win> <bool>)
	interp.Def(fnSetWindowFullScreen, 2, func(a []any) any {
		win := mustGet(fnSetWindowFullScreen, "GUI window ID", a, 0).(fyne.Window)
		win.SetFullScreen(z3.ToBool(a[1]))
		return z3.Void
	})

	fnWindowFullScreen := pre("window-full-screen?")
	// (window-full-screen? <win>) => bool
	interp.Def(fnWindowFullScreen, 1, func(a []any) any {
		win := mustGet(fnWindowFullScreen, "GUI window ID", a, 0).(fyne.Window)
		return z3.AsLispBool(win.FullScreen())
	})

	fnRequestWindowFocus := pre("request-window-focus")
	// (request-window-focus <win>)
	interp.Def(fnRequestWindowFocus, 1, func(a []any) any {
		win := mustGet(fnRequestWindowFocus, "GUI window ID", a, 0).(fyne.Window)
		win.RequestFocus()
		return z3.Void
	})

	fnSetWindowFixedSize := pre("set-window-fized-size")
	// (set-window-fixed-size <win> <bool>)
	interp.Def(fnSetWindowFixedSize, 2, func(a []any) any {
		win := mustGet(fnSetWindowFixedSize, "GUI window ID", a, 0).(fyne.Window)
		win.SetFixedSize(z3.ToBool(a[1]))
		return z3.Void
	})

	fnWindowFixedSize := pre("window-fixed-size?")
	// (window-fixed-size? <win>) => bool
	interp.Def(fnWindowFixedSize, 1, func(a []any) any {
		win := mustGet(fnWindowFixedSize, "GUI window ID", a, 0).(fyne.Window)
		return z3.AsLispBool(win.FixedSize())
	})

	fnCenterWindowOnScreen := pre("center-window-on-screen")
	// (center-window-on-screen <win>)
	interp.Def(fnCenterWindowOnScreen, 1, func(a []any) any {
		win := mustGet(fnCenterWindowOnScreen, "GUI window ID", a, 0).(fyne.Window)
		win.CenterOnScreen()
		return z3.Void
	})

	// (set-window-padded <win> <bool>)
	interp.Def(pre("set-window-padded"), 2, func(a []any) any {
		win := mustGet(pre("set-window-padded"), "GUI window ID", a, 0).(fyne.Window)
		win.SetPadded(z3.ToBool(a[1]))
		return z3.Void
	})

	fnWindowPadded := pre("window-padded?")
	// (window-padded? <win>) => bool
	interp.Def(fnWindowPadded, 1, func(a []any) any {
		win := mustGet(fnWindowPadded, "GUI window ID", a, 0).(fyne.Window)
		return z3.AsLispBool(win.Padded())
	})

	fnSetWindowIcon := pre("set-window-icon")
	// (set-window-icon <win> <icon>)
	interp.Def(fnSetWindowIcon, 2, func(a []any) any {
		win := mustGet(fnSetWindowIcon, "GUI window ID", a, 0).(fyne.Window)
		icon := mustGet(fnSetWindowIcon, "GUI resource ID", a, 1).(fyne.Resource)
		win.SetIcon(icon)
		return z3.Void
	})

	fnGetWindowIcon := pre("get-window-icon")
	// (get-window-icon <win>)
	interp.Def(fnGetWindowIcon, 1, func(a []any) any {
		win := mustGet(fnGetWindowIcon, "GUI window ID", a, 0).(fyne.Window)
		icon := win.Icon()
		id, ok := getID(icon)
		if ok {
			return id
		}
		return put(icon)
	})

	fnSetWindowMainMenu := pre("set-window-main-menu")
	// (set-window-main-menu <win> <menu>)
	interp.Def(fnSetWindowMainMenu, 2, func(a []any) any {
		win := mustGet(fnSetWindowMainMenu, "GUI window ID", a, 0).(fyne.Window)
		menu := mustGet(fnSetWindowMainMenu, "GUI main menu ID", a, 1).(*fyne.MainMenu)
		win.SetMainMenu(menu)
		return z3.Void
	})

	fnGetWindowMainMenu := pre("get-window-main-menu")
	// (get-window-main-menu <win>) => main menu ID
	interp.Def(fnGetWindowMainMenu, 1, func(a []any) any {
		win := mustGet(fnGetWindowMainMenu, "GUI window ID", a, 0).(fyne.Window)
		menu := win.MainMenu()
		id, ok := getID(menu)
		if ok {
			return id
		}
		return put(menu)
	})

	// LABEL

	fnNewLabel := pre("new-label")
	// (new-label <string>)
	interp.Def(fnNewLabel, 1, func(a []any) any {
		label := widget.NewLabel(a[0].(string))
		label.Wrapping = fyne.TextWrapWord
		return put(label)
	})

	fnSetLabelText := pre("set-label-text")
	// (set-label-text <label> <string>)
	interp.Def(fnSetLabelText, 2, func(a []any) any {
		label := mustGet(fnSetLabelText, "GUI label ID", a, 0)
		label.(*widget.Label).SetText(a[1].(string))
		return z3.Void
	})

	fnGetLabelText := pre("get-label-text")
	// (get-label-text label) => str
	interp.Def(fnGetLabelText, 1, func(a []any) any {
		label := mustGet(fnGetLabelText, "GUI label ID", a, 0).(*widget.Label)
		return label.Text
	})

	// ENTRY

	fnNewEntry := pre("new-entry")
	// (new-entry [<selector>])
	interp.Def(fnNewEntry, -1, func(a []any) any {
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
			panic(fmt.Sprintf(fnNewEntry+": unknown selector '%v, must be one of '(single-line multi-line password)",
				sort))
		}
	})

	fnSetEntryOnChangeCallback := pre("set-entry-on-change-callback")
	// (set-entry-on-change-callback <entry> (lambda (str) ...))
	interp.Def(fnSetEntryOnChangeCallback, 2, func(a []any) any {
		e := mustGet(fnSetEntryOnChangeCallback, "GUI entry ID", a, 0)
		proc := a[1].(*z3.Closure)
		e.(*widget.Entry).OnChanged = func(s string) {
			li2 := &z3.Cell{Car: s, Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.SafeEvalWithInfo(li, z3.Nil,
				"%v\n"+fmt.Sprintf("IN entry %v on-change-callback", z3.Str(a[0])))
		}
		return z3.Void
	})

	fnSetEntryTextWrap := pre("set-entry-text-wrap")
	// (set-entry-text-wrap entry selector)
	interp.Def(fnSetEntryTextWrap, 2, func(a []any) any {
		e := mustGet(fnSetEntryTextWrap, "GUI entry ID", a, 0).(*widget.Entry)
		wrap := a[1].(*z3.Sym)
		mode := MustConvertSymToTextWrap(fnSetEntryTextWrap, wrap)
		e.Wrapping = mode
		return z3.Void
	})

	fnSetEntryValidator := pre("set-entry-validator")
	// (set-entry-validator <entry> <validator>))
	interp.Def(fnSetEntryValidator, 2, func(a []any) any {
		e := mustGet(fnSetEntryValidator, "GUI entry ID", a, 0).(*widget.Entry)
		validator := mustGet(fnSetEntryValidator, "GUI validator ID", a, 1).(*validatorWrapper)
		e.Validator = validator.fn
		return z3.Void
	})

	fnEntryAcceptsTab := pre("entry-accepts-tab?")
	// (entry-accepts-tab? <entry>) => bool
	interp.Def(fnEntryAcceptsTab, 1, func(a []any) any {
		e := mustGet(fnEntryAcceptsTab, "entry", a, 0).(*widget.Entry)
		return z3.AsLispBool(e.AcceptsTab())
	})

	fnGetEntryCursorPos := pre("get-entry-cursor-pos")
	// (get-entry-cursor-pos <entry>) => li
	interp.Def(fnGetEntryCursorPos, 1, func(a []any) any {
		e := mustGet(fnGetEntryCursorPos, "GUI entry ID", a, 0).(*widget.Entry)
		row := e.CursorRow
		column := e.CursorColumn
		return &z3.Cell{Car: goarith.AsNumber(row), Cdr: &z3.Cell{Car: goarith.AsNumber(column), Cdr: z3.Nil}}
	})

	fnSetEntryCursorRow := pre("set-entry-cursor-row")
	// (set-entry-cursor-row <entry> <row>)
	interp.Def(fnSetEntryCursorRow, 2, func(a []any) any {
		e := mustGet(fnSetEntryCursorRow, "GUI entry ID", a, 0).(*widget.Entry)
		n := z3.ToInt64(fnSetEntryCursorRow, a[1])
		e.CursorRow = int(n)
		return z3.Void
	})

	fnSetEntryCursorColumn := pre("set-entry-cursor-column")
	// (set-entry-cursor-column <entry> <column>)
	interp.Def(fnSetEntryCursorColumn, 2, func(a []any) any {
		e := mustGet(fnSetEntryCursorColumn, "GUI entry ID", a, 0).(*widget.Entry)
		n := z3.ToInt64(fnSetEntryCursorColumn, a[1])
		e.CursorColumn = int(n)
		return z3.Void
	})

	fnSetEntryOnCursorChangeCallback := pre("set-entry-on-cursor-change-callback")
	// (set-entry-on-cursor-change-callback <entry> <proc>) where <proc> takes an entry ID as argument
	interp.Def(fnSetEntryOnCursorChangeCallback, 2, func(a []any) any {
		e := mustGet(fnSetEntryOnCursorChangeCallback, "GUI entry ID", a, 0).(*widget.Entry)
		proc := a[1].(*z3.Closure)
		e.OnCursorChanged = func() {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: a[0], Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN entry %v on-cursor-change-callback", z3.Str(a[0])))
		}
		return z3.Void
	})

	fnGetEntryCursor := pre("get-entry-cursor")
	// (get-entry-cursor <entry>) => sym
	interp.Def(fnGetEntryCursor, 1, func(a []any) any {
		e := mustGet(fnGetEntryCursor, "entry", a, 0).(*widget.Entry)
		return CursorToSym(e.Cursor())
	})

	fnGetEntrySelectedText := pre("get-entry-selected-text")
	// (get-entry-selected-text <entry>) => str
	interp.Def(fnGetEntrySelectedText, 1, func(a []any) any {
		e := mustGet(fnGetEntrySelectedText, "entry", a, 0).(*widget.Entry)
		return e.SelectedText()
	})

	fnSetEntryMinRowsVisible := pre("set-entry-min-rows-visible")
	// (set-entry-min-rows-visible <entry> <row>)
	interp.Def(fnSetEntryMinRowsVisible, 2, func(a []any) any {
		e := mustGet(fnSetEntryMinRowsVisible, "entry", a, 0).(*widget.Entry)
		n := z3.ToInt64(fnSetEntryMinRowsVisible, a[1])
		e.SetMinRowsVisible(int(n))
		return z3.Void
	})

	fnSetEntryPlaceHolder := pre("set-entry-place-holder")
	// (set-entry-place-holder <entry> <str>)
	interp.Def(fnSetEntryPlaceHolder, 2, func(a []any) any {
		e := mustGet(fnSetEntryPlaceHolder, "entry", a, 0).(*widget.Entry)
		e.SetPlaceHolder(a[1].(string))
		return z3.Void
	})

	fnSetEntryText := pre("set-entry-text")
	// (set-entry-text <entry> <str>)
	interp.Def(fnSetEntryText, 2, func(a []any) any {
		e := mustGet(fnSetEntryText, "entry", a, 0).(*widget.Entry)
		e.SetText(a[1].(string))
		return z3.Void
	})

	// VALIDATORS

	fnNewCombinedStringValidator := pre("new-combined-string-validator")
	// (new-combined-string-validators <validator> [<validators>...]) => validator ID
	interp.Def(fnNewCombinedStringValidator, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		validators := make([]fyne.StringValidator, 0)
		for li != z3.Nil {
			validator := mustGet1(fnNewCombinedStringValidator, "GUI string validator ID", li.Car)
			validators = append(validators, validator.(fyne.StringValidator))
			li = li.CdrCell()
		}
		return put(&validatorWrapper{fn: validation.NewAllStrings(validators...)})
	})

	fnNewRegexpValidator := pre("new-regexp-validator")
	// (new-regexp-validator <regexp-str> <reason>) => validator ID
	interp.Def(fnNewRegexpValidator, 2, func(a []any) any {
		return put(&validatorWrapper{validation.NewRegexp(a[0].(string), a[1].(string))})
	})

	fnNewTimeValidator := pre("new-time-validator")
	// (new-time-validator <time-format-str>) => validator ID
	interp.Def(fnNewTimeValidator, 1, func(a []any) any {
		return put(&validatorWrapper{fn: validation.NewTime(a[0].(string))})
	})

	fnNewValidator := pre("new-validator")
	// (new-validator <proc>) => validator ID, where <proc> is a function that takes a string
	// and returns a string. If the return string is not "", the validation fails with the reason
	// given in the string. If <proc> panics, validation also fails.
	interp.Def(fnNewValidator, 1, func(a []any) any {
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

	fnSetObjectOnValidationChangeCallback := pre("set-object-on-validation-change-callback")
	// (set-object-on-validation-change-callback <obj> <proc>) where <proc> takes a string as error message
	// and this argument is Nil if validation succeeds.
	interp.Def(fnSetObjectOnValidationChangeCallback, 2, func(a []any) any {
		e := mustGet(fnSetObjectOnValidationChangeCallback, "GUI validatable ID", a, 0).(fyne.Validatable)
		proc := a[1].(*z3.Closure)
		e.SetOnValidationChanged(func(e error) {
			var arg any
			if e == nil {
				arg = z3.Nil
			} else {
				arg = e.Error()
			}
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: arg, Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN validatable object %v on-validation-change callback", z3.Str(a[0])))
		})
		return z3.Void
	})

	fnValidateObject := pre("validate-object")
	// (validate-object <obj>) => error message string or ""
	interp.Def(fnValidateObject, 1, func(a []any) any {
		e := mustGet(fnValidateObject, "GUI validatable ID", a, 0).(fyne.Validatable)
		err := e.Validate()
		if err == nil {
			return ""
		}
		return err.Error()
	})

	// TEXTGRID

	fnNewTextGrid := pre("new-text-grid")
	// (new-text-grid [<string>] [show-line-numbers|show-whitespace|tab-width <int>]) => int
	interp.Def(fnNewTextGrid, -1, func(a []any) any {
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
							panic(fnNewTextGrid + ": expected a width integer after 'tab-width, but it is missing!")
						}
						grid.TabWidth = int(z3.ToInt64(pre("new-text-grid"), li.Car))
					}
				} else {
					panic(fmt.Sprintf(fnNewTextGrid+
						": expected an initial string as content and/or a selector in '(show-line-numbers show-whitespace tab-width), given %v",
						z3.Str(li.Car)))
				}
				li = li.CdrCell()
			}
		}
		return put(grid)
	})

	fnGetTextGridCellSize := pre("get-text-grid-cell-size")
	// (get-text-grid-cell-size grid) => li
	interp.Def(fnGetTextGridCellSize, 1, func(a []any) any {
		_, ok = mustGet(fnGetTextGridCellSize, "GUI text grid ID", a, 0).(*widget.TextGrid)
		if !ok {
			panic(fmt.Sprintf(fnGetTextGridCellSize+"get-text-grid-cell-size: expected text grid as argument, given %v", z3.Str(a[0])))
		}
		// unfortunately text-grid cell size and line calculations are internal, as of Fyne v2.4
		// the following is taken from textgrid.go: (t *TextGridRenderer) updateCellSize():
		size := fyne.MeasureText("M", theme.TextSize(), fyne.TextStyle{Monospace: true})
		// round it for seamless background
		w := math.Round(float64((size.Width)))
		h := math.Round(float64((size.Height)))
		return &z3.Cell{Car: goarith.AsNumber(w), Cdr: &z3.Cell{Car: goarith.AsNumber(h), Cdr: z3.Nil}}
	})

	fnTextGridShowLineNumbers := pre("text-grid-show-line-numbers?")
	// (text-grid-show-line-numbers? <grid>) => bool
	interp.Def(fnTextGridShowLineNumbers, 1, func(a []any) any {
		grid := mustGet(fnTextGridShowLineNumbers, "GUI text grid ID", a, 0).(*widget.TextGrid)
		return z3.AsLispBool(grid.ShowLineNumbers)
	})

	fnTextGridShowWhitespace := pre("text-grid-show-whitespace?")
	// (text-grid-show-whitespace? <grid>) => bool
	interp.Def(fnTextGridShowWhitespace, 1, func(a []any) any {
		grid := mustGet(fnTextGridShowWhitespace, "GUI text grid ID", a, 0).(*widget.TextGrid)
		return z3.AsLispBool(grid.ShowWhitespace)
	})

	fnTextGridTabWidth := pre("get-text-grid-tab-width")
	// (text-grid-tab-width <grid>) => num
	interp.Def(fnTextGridTabWidth, 1, func(a []any) any {
		grid := mustGet(fnTextGridTabWidth, "GUI text grid ID", a, 0).(*widget.TextGrid)
		return goarith.AsNumber(grid.TabWidth)
	})

	fnSetTextGridTabWidth := pre("set-text-grid-tab-width")
	// (set-text-grid-tab-width <grid> <n>)
	interp.Def(fnSetTextGridTabWidth, 2, func(a []any) any {
		grid := mustGet(fnSetTextGridTabWidth, "GUI text grid ID", a, 0).(*widget.TextGrid)
		grid.TabWidth = int(z3.ToInt64(fnSetTextGridTabWidth, a[1]))
		return z3.Void
	})

	fnSetTextGridShowLineNumbers := pre("set-text-grid-show-line-numbers")
	// (set-text-grid-show-line-numbers <grid> <show?>)
	interp.Def(fnSetTextGridShowLineNumbers, 2, func(a []any) any {
		grid := mustGet(fnSetTextGridShowLineNumbers, "GUI text grid ID", a, 0).(*widget.TextGrid)
		grid.ShowLineNumbers = z3.ToBool(a[1])
		return z3.Void
	})

	fnSetTextGridShowWhitespace := pre("set-text-grid-show-whitespace")
	// (set-text-grid-show-whitespace <grid> <show?>)
	interp.Def(fnSetTextGridShowWhitespace, 2, func(a []any) any {
		grid := mustGet(fnSetTextGridShowWhitespace, "GUI text grid ID", a, 0).(*widget.TextGrid)
		grid.ShowWhitespace = z3.ToBool(a[1])
		return z3.Void
	})

	fnGetTextGridRow := pre("get-text-grid-row")
	// (get-text-grid-row <grid> <row>) => li
	// The list consists of an array of cells and a style list. The array of cells contains lists
	// consisting of a rune string and a style list. Style lists consist of a list of foreground color
	// values and a list of background color values.
	interp.Def(fnGetTextGridRow, 2, func(a []any) any {
		grid := mustGet(fnGetTextGridRow, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := grid.Row(int(z3.ToInt64(fnGetTextGridRow, a[1])))
		cells := make([]any, 0)
		for _, cell := range row.Cells {
			elem := &z3.Cell{Car: string(cell.Rune), Cdr: &z3.Cell{Car: TextGridStyleToList(cell.Style),
				Cdr: z3.Nil}}
			cells = append(cells, elem)
		}
		return &z3.Cell{Car: cells, Cdr: &z3.Cell{Car: TextGridStyleToList(row.Style), Cdr: z3.Nil}}
	})

	fnGetTextGridRowText := pre("get-text-grid-row-text")
	// (get-text-grid-row-text <grid> <row>) => str
	interp.Def(fnGetTextGridRowText, 2, func(a []any) any {
		grid := mustGet(fnGetTextGridRowText, "GUI text grid ID", a, 0).(*widget.TextGrid)
		s := grid.RowText(int(z3.ToInt64(fnGetTextGridRowText, a[1])))
		return s
	})

	fnSetTextGridCell := pre("set-text-grid-cell")
	// (set-text-grid-cell <grid> <row> <column> <cell>) where <cell> is a list consisting
	// of a rune string and a text grid style list.
	interp.Def(fnSetTextGridCell, 4, func(a []any) any {
		grid := mustGet(fnSetTextGridCell, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnSetTextGridCell, a[1]))
		column := int(z3.ToInt64(fnSetTextGridCell, a[2]))
		li := a[3].(*z3.Cell)
		r := []rune(li.Car.(string))[0]
		li = li.CdrCell()
		style := ListToTextGridStyle(fnSetTextGridCell, li)
		grid.SetCell(row, column, widget.TextGridCell{Rune: r, Style: style})
		return z3.Void
	})

	fnGetTextGridCell := pre("get-text-grid-cell")
	// (get-text-grid-cell <grid> <row> <column>) => li
	interp.Def(fnGetTextGridCell, 3, func(a []any) any {
		grid := mustGet(fnGetTextGridCell, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnGetTextGridCell, a[1]))
		column := int(z3.ToInt64(fnGetTextGridCell, a[2]))
		gridRow := grid.Rows[row]
		cell := gridRow.Cells[column]
		return &z3.Cell{Car: string(cell.Rune), Cdr: &z3.Cell{Car: TextGridStyleToList(cell.Style),
			Cdr: z3.Nil}}
	})

	fnGetTextGridRune := pre("get-text-grid-rune")
	// (get-text-grid-rune grid row column) => str
	interp.Def(fnGetTextGridRune, 3, func(a []any) any {
		grid := mustGet(fnGetTextGridRune, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnGetTextGridRune, a[1]))
		column := int(z3.ToInt64(fnGetTextGridRune, a[2]))
		gridRow := grid.Rows[row]
		cell := gridRow.Cells[column]
		return string(cell.Rune)
	})

	fnSetTextGridRow := pre("set-text-grid-row")
	// (set-text-grid-row <grid> <row> <rowspec>) where <rowspec> has the same format as is returned
	// by get-text-grid-row, i.e. it is an array of cell lists containing a rune string and a style list.
	interp.Def(fnSetTextGridRow, 3, func(a []any) any {
		grid := mustGet(fnSetTextGridRow, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnSetTextGridRow, a[1]))
		li := a[2].(*z3.Cell)
		columns := li.Car.([]any)
		li = li.CdrCell()
		style := ListToTextGridStyle(fnSetTextGridRow, li.Car)
		cells := make([]widget.TextGridCell, 0, len(columns))
		for i := range columns {
			li = columns[i].(*z3.Cell)
			r := []rune(li.Car.(string))[0]
			li = li.CdrCell()
			sty := ListToTextGridStyle(fnSetTextGridRow, li.Car)
			cells = append(cells, widget.TextGridCell{Rune: r, Style: sty})
		}
		grid.SetRow(row, widget.TextGridRow{Cells: cells, Style: style})
		return z3.Void
	})

	fnWrapDeleteTextGrid := pre("wrap-delete-text-grid")
	// (wrap-delete-text-grid grid range-list wrapcol soft-wrap? hard-lf-rune soft-lf-rune cursor-row cursor-column) => li
	// Delete the given range of the form (startrow endrow endrow endcol) (all inclusive) while ensuring
	// that all paragraphs in the range are word wrapped. Returns a new cursor position.
	interp.Def(fnWrapDeleteTextGrid, 8, func(a []any) any {
		grid := mustGet(fnWrapDeleteTextGrid, "GUI text grid ID", a, 0).(*widget.TextGrid)
		startRow, startCol, endRow, endCol := TextGridRangeListToRange(fnWrapDeleteTextGrid, a[1].(*z3.Cell))
		wrapCol := z3.ToInt(fnWrapDeleteTextGrid, a[2])
		softWrap := z3.ToBool(a[3])
		hardLF := []rune(a[4].(string))[0]
		softLF := []rune(a[5].(string))[0]
		cursorRow := z3.ToInt(fnWrapDeleteTextGrid, a[6])
		cursorColumn := z3.ToInt(fnWrapDeleteTextGrid, a[7])
		// delete the range from startRow to endRow in the grid
		var underflow []widget.TextGridCell
		for i := endRow; i >= startRow; i-- {
			if startRow < i && i < endRow {
				grid.Rows = slices.Delete(grid.Rows, i, i+1)
				continue
			}
			row := grid.Rows[i]
			if i == startRow && i == endRow {
				row.Cells = slices.Delete(row.Cells, startCol, endCol)
				if cursorRow == i && cursorColumn >= startCol {
					cursorColumn = startCol
				}
			} else if i == startRow {
				row.Cells = slices.Delete(row.Cells, startCol, len(row.Cells))
				if underflow != nil {
					row.Cells = append(row.Cells, underflow...)
					grid.SetRow(i, row)
					cursorColumn = len(row.Cells) - len(underflow)
					grid.Rows = slices.Delete(grid.Rows, i, i+1)
					underflow = nil
				}
				if len(row.Cells) == 0 && i > 0 {
					grid.Rows = slices.Delete(grid.Rows, i, i+1)
					cursorColumn = 0
					cursorRow = i
				} else if inSelectionRange(startRow, startCol, endRow, endCol, cursorRow, cursorColumn) {
					cursorColumn = startCol
					cursorRow = i
				}
			} else if i == endRow {
				if endCol < len(row.Cells) {
					underflow = slices.Clone(row.Cells[endCol:])
				}
				row.Cells = slices.Delete(row.Cells, 0, endCol)
			}
			grid.SetRow(i, row)
		}
		// now we reflow with word wrap like in wrap-insert-text-grid
		paraStart := FindTextGridParagraphStart(grid, startRow, hardLF)
		paraEnd := FindTextGridParagraphEnd(grid, startRow, hardLF)
		rows := make([]widget.TextGridRow, paraEnd-paraStart+1)
		for i := range rows {
			rows[i] = grid.Row(i + paraStart)
		}
		newCursorRow := cursorRow
		newCursorCol := cursorColumn
		rows, newCursorRow, newCursorCol = WordWrapTextGridRows(rows, wrapCol, softWrap, hardLF, softLF, newCursorRow-paraStart, newCursorCol)
		// check if we need to delete rows
		if len(rows) < paraEnd-paraStart+1 {
			grid.Rows = slices.Delete(grid.Rows, paraStart+len(rows), paraEnd+1)
		}
		// check if we need to insert additional rows
		if len(rows) > paraEnd-paraStart+1 {
			newRows := make([]widget.TextGridRow, len(rows)-(paraEnd-paraStart+1))
			grid.Rows = slices.Insert(grid.Rows, paraEnd+1, newRows...)
		}
		for i := range rows {
			grid.SetRow(i+paraStart, rows[i])
		}
		return &z3.Cell{Car: goarith.AsNumber(paraStart + newCursorRow),
			Cdr: &z3.Cell{Car: goarith.AsNumber(newCursorCol), Cdr: z3.Nil}}
	})

	fnWrapInsertTextGrid := pre("wrap-insert-text-grid")
	// (wrap-insert-text-grid grid cells row col wrapcol soft-wrap? hard-lf-rune soft-lf-rune) => li
	// Soft or hard wrap a paragraph in which row, col is located, based on hard-lf-rune and soft-lf-rune
	// markers for end-of-line (since "\n" is not a good candidate for this, other markers are normally used).
	// The function returns a list of the form (row col) that represents the new cursor position if
	// row and col are the old cursor positions.
	interp.Def(fnWrapInsertTextGrid, 8, func(a []any) any {
		grid := mustGet(fnWrapInsertTextGrid, "GUI text grid ID", a, 0).(*widget.TextGrid)
		// cells are an array of any containing lists of the form (rune text-grid-style-list)
		// these are converted to an array of TextGridCell like in a TextGridRow
		cells := a[1].([]any)
		tgCells := make([]widget.TextGridCell, len(cells))
		for i := range cells {
			tgCells[i] = ListToTextGridCell(fnWrapInsertTextGrid, cells[i])
		}
		row := int(z3.ToInt64(fnWrapInsertTextGrid, a[2]))
		col := int(z3.ToInt64(fnWrapInsertTextGrid, a[3]))
		wrapCol := int(z3.ToInt64(fnWrapInsertTextGrid, a[4]))
		softWrap := z3.ToBool(a[5])
		hardLF := []rune(a[6].(string))[0]
		softLF := []rune(a[7].(string))[0]
		// we now obtain the paragraph start and end row and store the rows in an array
		// these are wrapped and the new wrapped rows replace the original rows
		// what we do in what follows has heavy performance implications
		startRow := FindTextGridParagraphStart(grid, row, hardLF)
		endRow := FindTextGridParagraphEnd(grid, row, hardLF)
		// log.Printf("startRow=%v, endRow=%v\n", startRow, endRow)
		rows := make([]widget.TextGridRow, (endRow-startRow)+1)
		for i := range rows {
			rows[i] = grid.Row(i + startRow)
		}
		k := row - startRow // the row into which we insert
		line := rows[k].Cells
		lenLine := len(line)
		lenInsert := len(tgCells)
		n := lenLine + lenInsert
		newLine := make([]widget.TextGridCell, 0, n)
		if col >= lenLine {
			newLine = append(newLine, line...)
			newLine = append(newLine, tgCells...)
		} else if col == 0 {
			newLine = append(newLine, tgCells...)
			newLine = append(newLine, line...)
		} else {
			newLine = append(newLine, line[:col]...)
			newLine = append(newLine, tgCells...)
			newLine = append(newLine, line[col:lenLine]...)
		}
		rows[k] = widget.TextGridRow{Cells: newLine, Style: rows[k].Style}
		// We now have rows with the text insert correctly into rows[k].
		// To achieve this, we extract all cells into one array, work on this
		// and then put the cells back into the rows. This is obviously not very efficient,
		// but correctness is more important for now.
		newCursorRow := row
		newCursorCol := col
		rows, newCursorRow, newCursorCol = WordWrapTextGridRows(rows, wrapCol, softWrap, hardLF, softLF, row-startRow, col)
		// check if we need to delete rows
		if len(rows) < endRow-startRow+1 {
			grid.Rows = slices.Delete(grid.Rows, startRow+len(rows), endRow+1)
		}
		// check if we need to insert additional rows
		if len(rows) > endRow-startRow+1 {
			newRows := make([]widget.TextGridRow, len(rows)-(endRow-startRow+1))
			grid.Rows = slices.Insert(grid.Rows, endRow+1, newRows...)
		}
		for i := range rows {
			grid.SetRow(i+startRow, rows[i])
		}
		return &z3.Cell{Car: goarith.AsNumber(startRow + newCursorRow),
			Cdr: &z3.Cell{Car: goarith.AsNumber(newCursorCol), Cdr: z3.Nil}}
	})

	fnRemoveTextGridRow := pre("remove-text-grid-row")
	// (remove-text-grid-row grid row)
	interp.Def(fnRemoveTextGridRow, 2, func(a []any) any {
		grid := mustGet(fnRemoveTextGridRow, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnRemoveTextGridRow, a[1]))
		grid.Rows = append(grid.Rows[:row], grid.Rows[row+1:]...)
		return z3.Void
	})

	fnInsertTextGridRow := pre("insert-text-grid-row")
	// (insert-text-grid-row grid row)
	interp.Def(fnInsertTextGridRow, 2, func(a []any) any {
		grid := mustGet(fnInsertTextGridRow, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnInsertTextGridRow, a[1]))
		if row == len(grid.Rows) {
			grid.Rows = append(grid.Rows, widget.TextGridRow{})
			return z3.Void
		}
		grid.Rows = append(grid.Rows, widget.TextGridRow{})
		copy(grid.Rows[row+1:], grid.Rows[row:])
		grid.Rows[row] = widget.TextGridRow{}
		return z3.Void
	})

	fnSetTextGridRowStyle := pre("set-text-grid-row-style")
	// (set-text-grid-row-style <grid> <row> <style-list>) sets the whole row to the style list,
	// which contains a foreground color and a background color list.
	interp.Def(fnSetTextGridRowStyle, 3, func(a []any) any {
		grid := mustGet(fnSetTextGridRowStyle, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnSetTextGridRowStyle, a[1]))
		li := a[2].(*z3.Cell)
		style := ListToTextGridStyle(fnSetTextGridRowStyle, li)
		grid.SetRowStyle(row, style)
		return z3.Void
	})

	fnSetTextGridRune := pre("set-text-grid-rune")
	// (set-text-grid-rune <grid> <row> <column> <str>)
	interp.Def(fnSetTextGridRune, 4, func(a []any) any {
		grid := mustGet(fnSetTextGridRune, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnSetTextGridRune, a[1]))
		column := int(z3.ToInt64(fnSetTextGridRune, a[2]))
		r := []rune(a[3].(string))[0]
		grid.SetRune(row, column, r)
		return z3.Void
	})

	fnSetTextGridStyle := pre("set-text-grid-style")
	// (set-text-grid-style <grid> <row> <column> <style-list>)
	interp.Def(fnSetTextGridStyle, 4, func(a []any) any {
		grid := mustGet(fnSetTextGridStyle, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnSetTextGridStyle, a[1]))
		column := int(z3.ToInt64(fnSetTextGridStyle, a[2]))
		style := ListToTextGridStyle(fnSetTextGridStyle, a[3])
		grid.SetStyle(row, column, style)
		return z3.Void
	})

	fnSetTextGridStyleRange := pre("set-text-grid-style-range")
	// (set-text-grid-style-range <grid> <start-row> <start-column> <end-row> <end-column> <style-list>)
	interp.Def(fnSetTextGridStyleRange, 6, func(a []any) any {
		grid := mustGet(fnSetTextGridStyleRange, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row1 := int(z3.ToInt64(fnSetTextGridStyleRange, a[1]))
		column1 := int(z3.ToInt64(fnSetTextGridStyleRange, a[2]))
		row2 := int(z3.ToInt64(fnSetTextGridStyleRange, a[3]))
		column2 := int(z3.ToInt64(fnSetTextGridStyleRange, a[4]))
		style := ListToTextGridStyle(fnSetTextGridStyleRange, a[5])
		grid.SetStyleRange(row1, column1, row2, column2, style)
		return z3.Void
	})

	fnSetTextGridText := pre("set-text-grid-text")
	// (set-text-grid-text <grid> <str>)
	interp.Def(fnSetTextGridText, 2, func(a []any) any {
		grid := mustGet(fnSetTextGridText, "GUI text grid ID", a, 0).(*widget.TextGrid)
		s := a[1].(string)
		grid.SetText(s)
		return z3.Void
	})

	fnGetTextGridText := pre("get-text-grid-text")
	// (get-text-grid-text <grid>) => str
	interp.Def(fnGetTextGridText, 1, func(a []any) any {
		grid := mustGet(fnGetTextGridText, "GUI text grid ID", a, 0).(*widget.TextGrid)
		return grid.Text()
	})

	// (count-text-grid-rows grid) => int
	interp.Def(pre("count-text-grid-rows"), 1, func(a []any) any {
		grid := mustGet(pre("count-text-grid-rows"), "GUI text grid ID", a, 0).(*widget.TextGrid)
		return goarith.AsNumber(len(grid.Rows))
	})

	fnCountTextGridRowColumns := pre("count-text-grid-row-columns")
	// (count-text-grid-row-columns grid row) => int
	interp.Def(fnCountTextGridRowColumns, 2, func(a []any) any {
		grid := mustGet(fnCountTextGridRowColumns, "GUI text grid ID", a, 0).(*widget.TextGrid)
		row := int(z3.ToInt64(fnCountTextGridRowColumns, a[1]))
		return goarith.AsNumber(len(grid.Rows[row].Cells))
	})

	// ZEDIT

	fnNewZedit := pre("new-zedit")
	// (new-zedit columns lines canvas) => int
	interp.Def(fnNewZedit, 3, func(a []any) any {
		columns := z3.ToInt(fnNewZedit, a[0])
		lines := z3.ToInt(fnNewZedit, a[1])
		canvas := mustGet(fnNewZedit, "GUI canvas ID", a, 2).(fyne.Canvas)
		editor := zedit.NewEditor(columns, lines, canvas)
		editor.Refresh()
		return put(editor)
	})

	fnSetZeditEventHandler := pre("set-zedit-event-handler")
	// (set-zedit-event-handler editor sel proc)
	interp.Def(fnSetZeditEventHandler, 3, func(a []any) any {
		editor := mustGet(fnSetZeditEventHandler, "GUI zedit ID", a, 0).(*zedit.Editor)
		event := SymToEditorEvent(fnSetZeditEventHandler, a[1])
		proc := a[2].(*z3.Closure)
		editor.SetEventHandler(event, zedit.EventHandler(func(event zedit.EditorEvent, editor *zedit.Editor) {
			evt := EditorEventToSym(fnSetZeditEventHandler, event)
			ed := getIDOrPut(editor)
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: z3.QqQuote(evt), Cdr: &z3.Cell{Car: ed,
				Cdr: z3.Nil}}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN zedit %v event handler for event %v", z3.Str(ed), z3.Str(evt)))
		}))
		return z3.Void
	})

	fnGetZeditTextRange := pre("get-zedit-text-range")
	// (get-zedit-text-range editor range) => str
	interp.Def(fnGetZeditTextRange, 2, func(a []any) any {
		editor := mustGet(fnGetZeditTextRange, "GUI zedit ID", a, 0).(*zedit.Editor)
		interval := ListToCharInterval(fnGetZeditTextRange, a[1].(*z3.Cell))
		return editor.GetTextRange(interval)
	})

	fnZeditCurrentSelectionText := pre("zedit-current-selection-text")
	// (zedit-current-selection-text editor) => str
	interp.Def(fnZeditCurrentSelectionText, 1, func(a []any) any {
		editor := mustGet(fnZeditCurrentSelectionText, "GUI zedit ID", a, 0).(*zedit.Editor)
		return editor.CurrentSelectionText()
	})

	fnRemoveZeditEventHandler := pre("remove-zedit-event-handler")
	// (remove-zedit-event-handler editor sel)
	interp.Def(fnRemoveZeditEventHandler, 2, func(a []any) any {
		editor := mustGet(fnRemoveZeditEventHandler, "GUI zedit ID", a, 0).(*zedit.Editor)
		event := SymToEditorEvent(fnRemoveZeditEventHandler, a[1])
		editor.RemoveEventHandler(event)
		return z3.Void
	})

	fnFocusZedit := pre("focus-zedit")
	// (focus-zedit editor)
	interp.Def(fnFocusZedit, 1, func(a []any) any {
		editor := mustGet(fnFocusZedit, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.Focus()
		return z3.Void
	})

	fnMakeOrGetZeditStyleTag := pre("make-or-get-zedit-style-tag")
	// (make-or-get-zedit-Style-tag editor style-list draw-full-line?)
	interp.Def(fnMakeOrGetZeditStyleTag, 3, func(a []any) any {
		editor := mustGet(fnMakeOrGetZeditStyleTag, "GUI zedit ID", a, 0).(*zedit.Editor)
		sty := ListToZeditStyle(fnMakeOrGetZeditStyleTag, a[1])
		tag := editor.MakeOrGetStyleTag(sty, z3.ToBool(a[2]))
		return getIDOrPut(tag)
	})

	fnGetZeditLines := pre("get-zedit-lines")
	// (get-zedit-lines editor) => int
	interp.Def(fnGetZeditLines, 1, func(a []any) any {
		editor := mustGet(fnGetZeditLines, "GUI zedit ID", a, 0).(*zedit.Editor)
		return goarith.AsNumber(editor.Lines)
	})

	fnGetZeditColumns := pre("get-zedit-columns")
	// (get-zedit-columns editor) => int
	interp.Def(fnGetZeditColumns, 1, func(a []any) any {
		editor := mustGet(fnGetZeditColumns, "GUI zedit ID", a, 0).(*zedit.Editor)
		return goarith.AsNumber(editor.Columns)
	})

	fnSetZeditConfig := pre("set-zedit-config")
	// (set-zedit-config editor selector prop)
	interp.Def(fnSetZeditConfig, 3, func(a []any) any {
		editor := mustGet(fnSetZeditConfig, "GUI zedit ID", a, 0).(*zedit.Editor)
		SetZeditConfig(interp, fnSetZeditConfig, editor, a[1], a[2])
		return z3.Void
	})

	fnGetZeditConfig := pre("get-zedit-config")
	// (get-zedit-prop editor selector) => any
	interp.Def(fnGetZeditConfig, 2, func(a []any) any {
		editor := mustGet(fnGetZeditConfig, "GUI zedit ID", a, 0).(*zedit.Editor)
		return GetZeditConfig(interp, fnGetZeditConfig, editor, a[1])
	})

	fnSetZeditText := pre("set-zedit-text")
	// (set-zedit-text <editor> <str>)
	interp.Def(fnSetZeditText, 2, func(a []any) any {
		editor := mustGet(fnSetZeditText, "GUI zedit ID", a, 0).(*zedit.Editor)
		s := a[1].(string)
		editor.SetText(s)
		return z3.Void
	})

	fnGetZeditText := pre("get-zedit-text")
	// (get-zedit-text <editor>) => str
	interp.Def(fnGetZeditText, 1, func(a []any) any {
		editor := mustGet(fnGetZeditText, "GUI zedit ID", a, 0).(*zedit.Editor)
		return editor.Text()
	})

	fnGetZeditLastLine := pre("get-zedit-last-line")
	// (get-zedit-last-line editor) => int
	interp.Def(fnGetZeditLastLine, 1, func(a []any) any {
		editor := mustGet(fnGetZeditLastLine, "GUI zedit ID", a, 0).(*zedit.Editor)
		return goarith.AsNumber(len(editor.Rows))
	})

	fnGetZeditLastColumn := pre("get-zedit-last-column")
	// (get-zedit-last-column grid line) => int
	interp.Def(fnGetZeditLastColumn, 2, func(a []any) any {
		editor := mustGet(fnGetZeditLastColumn, "GUI zedit ID", a, 0).(*zedit.Editor)
		row := int(z3.ToInt64(fnGetZeditLastColumn, a[1]))
		return goarith.AsNumber(editor.LastColumn(row))
	})

	fnSetZeditLineNumberStyle := pre("set-zedit-line-number-style")
	// (set-zedit-line-number-style editor style)
	interp.Def(fnSetZeditLineNumberStyle, 2, func(a []any) any {
		editor := mustGet(fnSetZeditLineNumberStyle, "GUI zedit ID", a, 0).(*zedit.Editor)
		style := ListToZeditStyle(fnSetZeditLineNumberStyle, a[1])
		editor.SetLineNumberStyle(style)
		return z3.Void
	})

	fnSetZeditTopLine := pre("set-zedit-top-line")
	// (set-zedit-top-line editor n)
	interp.Def(fnSetZeditTopLine, 2, func(a []any) any {
		editor := mustGet(fnSetZeditTopLine, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnSetZeditTopLine, a[1])
		editor.SetTopLine(n)
		return z3.Void
	})

	fnGetZeditTopLine := pre("get-zedit-top-line")
	// (get-zedit-top-line editor) => int
	interp.Def(fnGetZeditTopLine, 1, func(a []any) any {
		editor := mustGet(fnGetZeditTopLine, "GUI zedit ID", a, 0).(*zedit.Editor)
		return goarith.AsNumber(editor.TopLine())
	})

	fnCenterZeditLineOnCaret := pre("center-zedit-line-on-caret")
	// (center-zedit-line-on-caret editor)
	interp.Def(fnCenterZeditLineOnCaret, 1, func(a []any) any {
		editor := mustGet(fnCenterZeditLineOnCaret, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.CenterLineOnCaret()
		return z3.Void
	})

	fnGetZeditLineText := pre("get-zedit-line-text")
	// (get-zedit-line-text editor n) => str
	interp.Def(fnGetZeditLineText, 2, func(a []any) any {
		editor := mustGet(fnGetZeditLineText, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnGetZeditLineText, a[1])
		return editor.LineText(n)
	})

	fnSetZeditLineText := pre("set-zedit-line-text")
	// (set-zedit-line-text editor n s) => str
	interp.Def(fnSetZeditLineText, 3, func(a []any) any {
		editor := mustGet(fnSetZeditLineText, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnSetZeditLineText, a[1])
		s := a[2].(string)
		editor.SetLine(n, []rune(s))
		return z3.Void
	})

	fnFindZeditParagraphStart := pre("find-zedit-paragraph-start")
	// (find-zedit-paragraph-start editor line lf) => num
	interp.Def(fnFindZeditParagraphStart, 3, func(a []any) any {
		editor := mustGet(fnFindZeditParagraphStart, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnFindZeditParagraphStart, a[1])
		s := a[2].(string)
		return goarith.AsNumber(editor.FindParagraphStart(n, []rune(s)[0]))
	})

	fnFindZeditParagraphEnd := pre("find-zedit-paragraph-end")
	// (find-zedit-paragraph-end editor line lf) => num
	interp.Def(fnFindZeditParagraphEnd, 3, func(a []any) any {
		editor := mustGet(fnFindZeditParagraphEnd, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnFindZeditParagraphEnd, a[1])
		s := a[2].(string)
		return goarith.AsNumber(editor.FindParagraphEnd(n, []rune(s)[0]))
	})

	fnSetZeditMark := pre("set-zedit-mark")
	// (set-zedit-mark editor n)
	interp.Def(fnSetZeditMark, 2, func(a []any) any {
		editor := mustGet(fnSetZeditMark, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnSetZeditMark, a[1])
		if n < 0 || n >= len(editor.Config.MarkTags) {
			panic(fmt.Sprintf("%v: zedit mark index out of bounds, should be in [0...%v], given %v", fnSetZeditMark,
				len(editor.Config.MarkTags)-1, n))
		}
		editor.SetMark(n)
		return z3.Void
	})

	fnCutZedit := pre("cut-zedit")
	// (cut-zedit editor)
	interp.Def(fnCutZedit, 1, func(a []any) any {
		editor := mustGet(fnCutZedit, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.Cut()
		return z3.Void
	})

	fnScrollZeditDown := pre("scroll-zedit-down")
	// (scroll-zedit-down editor)
	interp.Def(fnScrollZeditDown, 1, func(a []any) any {
		editor := mustGet(fnScrollZeditDown, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.ScrollDown()
		return z3.Void
	})

	fnScrollZeditUp := pre("scroll-zedit-up")
	// (scroll-zedit-up editor)
	interp.Def(fnScrollZeditUp, 1, func(a []any) any {
		editor := mustGet(fnScrollZeditUp, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.ScrollUp()
		return z3.Void
	})

	fnScrollZeditLeft := pre("scroll-zedit-left")
	// (scroll-zedit-left editor n)
	interp.Def(fnScrollZeditLeft, 2, func(a []any) any {
		editor := mustGet(fnScrollZeditLeft, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnScrollZeditLeft, a[1])
		editor.ScrollLeft(n)
		return z3.Void
	})

	fnScrollZeditRight := pre("scroll-zedit-right")
	// (scroll-zedit-left editor n)
	interp.Def(fnScrollZeditRight, 2, func(a []any) any {
		editor := mustGet(fnScrollZeditRight, "GUI zedit ID", a, 0).(*zedit.Editor)
		n := z3.ToInt(fnScrollZeditRight, a[1])
		editor.ScrollRight(n)
		return z3.Void
	})

	fnGetZeditCurrentSelection := pre("get-zedit-current-selection")
	// (get-zedit-current-selection editor) => li
	interp.Def(fnGetZeditCurrentSelection, 1, func(a []any) any {
		editor := mustGet(fnGetZeditCurrentSelection, "GUI zedit ID", a, 0).(*zedit.Editor)
		sel, ok := editor.CurrentSelection()
		if !ok {
			return z3.Nil
		}
		return CharIntervalToList(sel)
	})

	fnSelectZeditWord := pre("select-zedit-word")
	// (select-zedit-word editor pos) => li
	interp.Def(fnSelectZeditWord, 2, func(a []any) any {
		editor := mustGet(fnSelectZeditWord, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := ListToCharPos(fnSelectZeditWord, a[1].(*z3.Cell))
		editor.SelectWord(pos)
		return z3.Void
	})

	fnRemoveZeditSelection := pre("remove-zedit-selection")
	// (remove-zedit-selection editor)
	interp.Def(fnRemoveZeditSelection, 1, func(a []any) any {
		editor := mustGet(fnRemoveZeditSelection, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.RemoveSelection()
		return z3.Void
	})

	fnConvertPosToCharPos := pre("convert-pos-to-zedit-charpos")
	// (convert-pos-to-zedit-charpos editor x y) => li
	interp.Def(fnConvertPosToCharPos, 3, func(a []any) any {
		editor := mustGet(fnConvertPosToCharPos, "GUI zedit ID", a, 0).(*zedit.Editor)
		x := z3.ToFloat64(a[1])
		y := z3.ToFloat64(a[2])
		pos := editor.PosToCharPos(fyne.Position{X: float32(x), Y: float32(y)})
		return CharPosToList(pos)
	})

	fnAddZeditShortcut := pre("add-zedit-shortcut")
	// (add-zedit-shortcut editor shortcut proc)
	interp.Def(fnAddZeditShortcut, 3, func(a []any) any {
		editor := mustGet(fnAddZeditShortcut, "GUI zedit ID", a, 0).(*zedit.Editor)
		key, modifier := MustGetShortcut(fnAddZeditShortcut, a[1].(*z3.Cell))
		proc := a[2].(*z3.Closure)
		shortcut := &desktop.CustomShortcut{KeyName: key, Modifier: modifier}
		s := zedit.GetKeyboardShortcutKey(shortcut)
		editor.AddShortcutHandler(shortcut, func(z *zedit.Editor) {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN canvas %v shortcut handler", z3.Str(a[0])))
		})
		return s
	})

	fnRemoveZeditShortcut := pre("remove-zedit-shortcut")
	// (remove-zedit-shortcut editor s)
	interp.Def(fnRemoveZeditShortcut, 2, func(a []any) any {
		editor := mustGet(fnRemoveZeditShortcut, "GUI zedit ID", a, 0).(*zedit.Editor)
		name := a[1].(string)
		editor.RemoveShortcutHandler(name)
		return z3.Void
	})

	fnAddZeditKeyHandler := pre("add-zedit-key")
	// (add-zedit-key editor key proc)
	interp.Def(fnAddZeditKeyHandler, 3, func(a []any) any {
		editor := mustGet(fnAddZeditKeyHandler, "GUI zedit ID", a, 0).(*zedit.Editor)
		var key string
		sym, ok := a[1].(*z3.Sym)
		if ok {
			key = sym.String()
		} else {
			key = a[1].(string)
		}
		proc := a[2].(*z3.Closure)
		editor.AddKeyHandler(fyne.KeyName(key), func(z *zedit.Editor) {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN canvas %v shortcut handler", z3.Str(a[0])))
		})
		return z3.Void
	})

	fnRemoveZeditKey := pre("remove-zedit-key")
	// (remove-zedit-key editor key)
	interp.Def(fnRemoveZeditKey, 2, func(a []any) any {
		editor := mustGet(fnRemoveZeditKey, "GUI zedit ID", a, 0).(*zedit.Editor)
		var key string
		sym, ok := a[1].(*z3.Sym)
		if ok {
			key = sym.String()
		} else {
			key = a[1].(string)
		}
		editor.RemoveKeyHandler(fyne.KeyName(key))
		return z3.Void
	})

	fnRefreshZedit := pre("refresh-zedit")
	// (refresh-zedit editor)
	interp.Def(fnRefreshZedit, 1, func(a []any) any {
		editor := mustGet(fnRefreshZedit, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.Refresh()
		return z3.Void
	})

	fnGetZeditCharAt := pre("get-zedit-char-at")
	// (get-zedit-char-at editor pos) => str
	interp.Def(fnGetZeditCharAt, 2, func(a []any) any {
		editor := mustGet(fnGetZeditCharAt, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := ListToCharPos(fnGetZeditCharAt, a[1].(*z3.Cell))
		c, ok := editor.CharAt(pos)
		if !ok {
			return ""
		}
		return string(c)
	})

	fnBlinkZeditCaret := pre("blink-zedit-caret")
	// (blink-zedit-caret editor on?)
	interp.Def(fnBlinkZeditCaret, 2, func(a []any) any {
		editor := mustGet(fnBlinkZeditCaret, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.BlinkCaret(z3.ToBool(a[1]))
		return z3.Void
	})

	fnHasBlinkZeditCaret := pre("zedit-caret-blinking?")
	// (zedit-caret-blinking? editor) => bool
	interp.Def(fnHasBlinkZeditCaret, 1, func(a []any) any {
		editor := mustGet(fnBlinkZeditCaret, "GUI zedit ID", a, 0).(*zedit.Editor)
		return z3.AsLispBool(editor.HasBlinkingCaret())
	})

	fnZeditCaretOff := pre("switch-zedit-caret-off")
	// (switch-zedit-caret-off editor) => bool
	interp.Def(fnZeditCaretOff, 1, func(a []any) any {
		editor := mustGet(fnZeditCaretOff, "GUI zedit ID", a, 0).(*zedit.Editor)
		blinking := editor.CaretOff()
		return z3.AsLispBool(blinking)
	})

	fnZeditCaretOn := pre("switch-zedit-caret-on")
	// (switch-zedit-caret-on editor on?)
	interp.Def(fnZeditCaretOn, 2, func(a []any) any {
		editor := mustGet(fnZeditCaretOn, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.CaretOn(z3.ToBool(a[1]))
		return z3.Void
	})

	fnSetZeditCaret := pre("set-zedit-caret")
	// (set-zedit-caret editor pos)
	interp.Def(fnSetZeditCaret, 2, func(a []any) any {
		editor := mustGet(fnSetZeditCaret, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := ListToCharPos(fnSetZeditCaret, a[1].(*z3.Cell))
		editor.SetCaret(pos)
		return z3.Void
	})

	fnGetZeditCaret := pre("get-zedit-caret")
	// (get-zedit-caret editor) => li
	interp.Def(fnGetZeditCaret, 1, func(a []any) any {
		editor := mustGet(fnGetZeditCaret, "GUI zedit ID", a, 0).(*zedit.Editor)
		return CharPosToList(editor.GetCaret())
	})

	fnMoveZeditCaret := pre("move-zedit-caret")
	// (move-zedit-caret editor selector)
	interp.Def(fnMoveZeditCaret, 2, func(a []any) any {
		editor := mustGet(fnMoveZeditCaret, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.MoveCaret(CaretMovementSymToCaretMovement(fnMoveZeditCaret, a[1].(*z3.Sym)))
		return z3.Void
	})

	fnGetZeditCurrentWord := pre("get-zedit-current-word")
	// (get-zedit-current-word editor) => str
	interp.Def(fnGetZeditCurrentWord, 1, func(a []any) any {
		editor := mustGet(fnGetZeditCurrentWord, "GUI zedit ID", a, 0).(*zedit.Editor)
		return editor.CurrentWord()
	})

	fnInsertZeditAt := pre("insert-zedit-at")
	// (insert-zedit-at editor s pos)
	interp.Def(fnInsertZeditAt, 3, func(a []any) any {
		editor := mustGet(fnInsertZeditAt, "GUI zedit ID", a, 0).(*zedit.Editor)
		s := a[1].(string)
		pos := ListToCharPos(fnInsertZeditAt, a[2].(*z3.Cell))
		editor.Insert([]rune(s), pos)
		return z3.Void
	})

	fnDeleteZeditRange := pre("delete-zedit-range")
	// (delete-zedit-range editor range)
	interp.Def(fnDeleteZeditRange, 2, func(a []any) any {
		editor := mustGet(fnDeleteZeditRange, "GUI zedit ID", a, 0).(*zedit.Editor)
		interval := ListToCharInterval(fnDeleteZeditRange, a[1].(*z3.Cell))
		editor.Delete(interval)
		return z3.Void
	})

	fnDeleteZeditAll := pre("delete-zedit-all")
	// (delete-zedit-all editor)
	interp.Def(fnDeleteZeditAll, 1, func(a []any) any {
		editor := mustGet(fnDeleteZeditAll, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.DeleteAll()
		return z3.Void
	})

	fnPrintZedit := pre("print-zedit")
	// (print-zedit editor text tagli)
	interp.Def(fnPrintZedit, 3, func(a []any) any {
		ed := mustGet1(fnPrintZedit, "GUI zedit ID", a[0]).(*zedit.Editor)
		s := a[1].(string)
		li := a[2].(*z3.Cell)
		if li == z3.Nil {
			ed.Print(s, nil)
			return z3.Void
		}
		tags := make([]zedit.Tag, 0)
		for li != z3.Nil {
			tag := mustGet1(fnPrintZedit, "GUI tag ID", li.Car).(zedit.Tag)
			tags = append(tags, tag)
			li = li.CdrCell()
		}
		ed.Print(s, tags)
		return z3.Void
	})

	fnCompleteZeditToEnd := pre("complete-zedit-to-end")
	// (complete-zedit-to-end editor start)
	interp.Def(fnCompleteZeditToEnd, 2, func(a []any) any {
		editor := mustGet(fnCompleteZeditToEnd, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := ListToCharPos(fnCompleteZeditToEnd, a[1].(*z3.Cell))
		interval := editor.ToEnd(pos)
		return CharIntervalToList(interval)
	})

	fnGetZeditLastPos := pre("get-zedit-last-pos")
	// (get-zedit-last-pos editor) => li
	interp.Def(fnGetZeditLastPos, 1, func(a []any) any {
		editor := mustGet(fnGetZeditLastPos, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := editor.LastPos()
		return CharPosToList(pos)
	})

	fnGetZeditPrevPos := pre("get-zedit-prev-pos")
	// (get-zedit-prev-pos editor pos) => li
	interp.Def(fnGetZeditPrevPos, 2, func(a []any) any {
		editor := mustGet(fnGetZeditPrevPos, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := ListToCharPos(fnGetZeditPrevPos, a[1].(*z3.Cell))
		prev, ok := editor.PrevPos(pos)
		return &z3.Cell{Car: CharPosToList(prev), Cdr: &z3.Cell{Car: z3.AsLispBool(ok), Cdr: z3.Nil}}
	})

	fnGetZeditNextPos := pre("get-zedit-next-pos")
	// (get-zedit-next-pos editor pos) => li
	interp.Def(fnGetZeditNextPos, 2, func(a []any) any {
		editor := mustGet(fnGetZeditNextPos, "GUI zedit ID", a, 0).(*zedit.Editor)
		pos := ListToCharPos(fnGetZeditNextPos, a[1].(*z3.Cell))
		next, ok := editor.NextPos(pos)
		return &z3.Cell{Car: CharPosToList(next), Cdr: &z3.Cell{Car: z3.AsLispBool(ok), Cdr: z3.Nil}}
	})

	fnZeditBackspace := pre("zedit-backspace")
	// (zedit-backspace editor)
	interp.Def(fnZeditBackspace, 1, func(a []any) any {
		editor := mustGet(fnZeditBackspace, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.Backspace()
		return z3.Void
	})

	fnZeditDelete1 := pre("zedit-delete1")
	// (zedit-delete1 editor)
	interp.Def(fnZeditDelete1, 1, func(a []any) any {
		editor := mustGet(fnZeditDelete1, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.Delete1()
		return z3.Void
	})

	fnZeditReturn := pre("zedit-return")
	// (zedit-return editor)
	interp.Def(fnZeditReturn, 1, func(a []any) any {
		editor := mustGet(fnZeditReturn, "GUI zedit ID", a, 0).(*zedit.Editor)
		editor.Return()
		return z3.Void
	})

	fnNewTag := pre("new-tag")
	// (new-tag name [index] [user-data])
	interp.Def(fnNewTag, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		name := li.Car.(string)
		li = li.CdrCell()
		if li == z3.Nil {
			return put(zedit.NewTag(name))
		}
		index := z3.ToInt(fnNewTag, li.Car)
		li = li.CdrCell()
		return put(zedit.NewTagWithUserData(name, index, li.Car))
	})

	fnGetTagName := pre("get-tag-name")
	// (get-tag-name tag) => str
	interp.Def(fnGetTagName, 1, func(a []any) any {
		tag := mustGet(fnGetTagName, "GUI tag ID", a, 0).(zedit.Tag)
		return tag.Name()
	})

	fnGetTagIndex := pre("get-tag-index")
	// (get-tag-index tag) => int
	interp.Def(fnGetTagIndex, 1, func(a []any) any {
		tag := mustGet(fnGetTagIndex, "GUI tag ID", a, 0).(zedit.Tag)
		return goarith.AsNumber(tag.Index())
	})

	fnGetTagUserData := pre("get-tag-user-data")
	// (get-tag-user-data tag) => any
	interp.Def(fnGetTagUserData, 1, func(a []any) any {
		tag := mustGet(fnGetTagUserData, "GUI tag ID", a, 0).(zedit.Tag)
		data := tag.UserData()
		if data == nil {
			return z3.Nil
		}
		return data
	})

	fnSetTagUserData := pre("set-tag-user-data")
	// (set-tag-user-data tag datum)
	interp.Def(fnSetTagUserData, 2, func(a []any) any {
		tag := mustGet(fnSetTagUserData, "GUI tag ID", a, 0).(zedit.Tag)
		tag.SetUserData(a[1])
		return z3.Void
	})

	fnSetTagCallback := pre("set-tag-callback")
	// (set-tag-callback tag proc)
	interp.Def(fnSetTagCallback, 2, func(a []any) any {
		tag := mustGet(fnSetTagCallback, "GUI tag ID", a, 0).(zedit.Tag)
		proc := a[1].(*z3.Closure)
		tag.SetCallback(zedit.TagFunc(func(evt zedit.TagEvent, tag zedit.Tag,
			interval zedit.CharInterval) {
			evtSym := TagEventToTagEventSymbol(evt)
			tagID, _ := getID(tag)
			li := CharIntervalToList(interval)
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: evtSym,
				Cdr: &z3.Cell{Car: tagID, Cdr: &z3.Cell{Car: li, Cdr: z3.Nil}}}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN canvas %v shortcut handler", z3.Str(a[0])))
		}))
		return z3.Void
	})

	fnAddZeditTags := pre("add-zedit-tags")
	// (add-zedit-tags editor range tags...)
	interp.Def(fnAddZeditTags, -1, func(a []any) any {
		arr := z3.ListToArray(a[0].(*z3.Cell))
		editor := mustGet(fnAddZeditTags, "GUI zedit ID", arr, 0).(*zedit.Editor)
		interval := ListToCharInterval(fnAddZeditTags, arr[1].(*z3.Cell))
		tags := make([]zedit.Tag, 0, len(arr)-2)
		for i := 2; i < len(arr); i++ {
			tag := mustGet(fnAddZeditTags, "GUI tag ID", arr, i)
			tags = append(tags, tag.(zedit.Tag))
		}
		editor.Tags.Add(interval, tags...)
		return z3.Void
	})

	fnDeleteZeditTag := pre("delete-zedit-tag")
	// (delete-zedit-tag editor tag)
	interp.Def(fnDeleteZeditTag, 2, func(a []any) any {
		editor := mustGet(fnDeleteZeditTag, "GUI zedit ID", a, 0).(*zedit.Editor)
		tag := mustGet(fnDeleteZeditTag, "GUI tag ID", a, 1).(zedit.Tag)
		editor.Tags.Delete(tag)
		return z3.Void
	})

	fnUpsertZeditTag := pre("upsert-zedit-tag")
	// (upsert-zedit-tag editor tag range)
	interp.Def(fnUpsertZeditTag, 3, func(a []any) any {
		editor := mustGet(fnUpsertZeditTag, "GUI zedit ID", a, 0).(*zedit.Editor)
		tag := mustGet(fnUpsertZeditTag, "GUI tag ID", a, 1).(zedit.Tag)
		interval := ListToCharInterval(fnUpsertZeditTag, a[2].(*z3.Cell))
		editor.Tags.Upsert(tag, interval)
		return z3.Void
	})

	fnGetZeditTagsByName := pre("get-zedit-tags-by-name")
	// (get-zedit-tags-by-name editor name)
	interp.Def(fnGetZeditTagsByName, 2, func(a []any) any {
		editor := mustGet(fnGetZeditTagsByName, "GUI zedit ID", a, 0).(*zedit.Editor)
		name := a[1].(string)
		set, ok := editor.Tags.TagsByName(name)
		if !ok {
			return z3.Nil
		}
		tags := set.Values()
		arr := make([]any, 0, len(tags))
		for i := range tags {
			id, ok := getID(tags[i])
			if ok {
				arr = append(arr, id)
			}
		}
		return z3.ArrayToList(arr)
	})

	fnCloneZeditTag := pre("clone-zedit-tag")
	// (clone-zedit-tag editor tag)
	interp.Def(fnCloneZeditTag, 2, func(a []any) any {
		editor := mustGet(fnCloneZeditTag, "GUI zedit ID", a, 0).(*zedit.Editor)
		tag := mustGet(fnCloneZeditTag, "GUI tag ID", a, 1).(zedit.Tag)
		return put(editor.Tags.CloneTag(tag))
	})

	fnNewZeditStyle := pre("new-zedit-style")
	// (new-style editor tag-name style blend? draw-full-line?)
	interp.Def(fnNewZeditStyle, 5, func(a []any) any {
		editor := mustGet(fnNewZeditStyle, "GUI zedit ID", a, 0).(*zedit.Editor)
		name := a[1].(string)
		style := ListToZeditStyle(fnNewZeditStyle, a[2])
		blend := z3.ToBool(a[3])
		drawFullLine := z3.ToBool(a[4])
		fn := zedit.TagStyleFunc(func(tag zedit.Tag, c zedit.Cell) zedit.Cell {
			fg := style.FGColor
			bg := style.BGColor
			if blend && c.Style != zedit.EmptyStyle {
				if c.Style.FGColor != nil {
					fg = zedit.BlendColors(editor.Config.BlendFG, editor.Config.BlendFGSwitched,
						c.Style.FGColor, fg)
				}
				if c.Style.BGColor != nil {
					bg = zedit.BlendColors(editor.Config.BlendBG, editor.Config.BlendBGSwitched,
						c.Style.BGColor, bg)
				}
			}
			newStyle := zedit.Style{FGColor: fg, BGColor: bg}
			return zedit.Cell{
				Rune:  c.Rune,
				Style: newStyle,
			}
		})
		return put(zedit.TagStyler{TagName: name, StyleFunc: fn, DrawFullLine: drawFullLine})
	})

	fnAddZeditStyle := pre("add-zedit-style")
	// (add-zedit-style editor styler)
	interp.Def(fnAddZeditStyle, 2, func(a []any) any {
		editor := mustGet(fnAddZeditStyle, "GUI zedit ID", a, 0).(*zedit.Editor)
		styler := mustGet(fnAddZeditStyle, "GUI zedit style ID", a, 1).(zedit.TagStyler)
		editor.Styles.AddStyler(styler)
		return z3.Void
	})

	fnRemoveZeditStyle := pre("remove-zedit-style")
	// (remove-zedit-style editor tag)
	interp.Def(fnRemoveZeditStyle, 2, func(a []any) any {
		editor := mustGet(fnRemoveZeditStyle, "GUI zedit ID", a, 0).(*zedit.Editor)
		tag := mustGet(fnRemoveZeditStyle, "GUI zedit style ID", a, 1).(zedit.Tag)
		editor.Styles.RemoveStyler(tag)
		return z3.Void
	})

	// CHECK

	fnNewCheck := pre("new-check")
	// (new-check <title-string> (lambda (bool) ...))
	interp.Def(fnNewCheck, 2, func(a []any) any {
		title := a[0].(string)
		proc := a[1].(*z3.Closure)
		id, zid := newID()
		changed := func(b bool) {
			li2 := &z3.Cell{Car: z3.AsLispBool(b), Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.SafeEvalWithInfo(li, z3.Nil, "%v\n"+fmt.Sprintf("IN check %v callback", z3.Str(zid)))
		}
		putWithID(id, widget.NewCheck(title, changed))
		return zid
	})

	// CHOICE

	fnNewChoice := pre("new-choice")
	// (new-choice <selector> <string-list> (lambda (str) ...))
	interp.Def(fnNewChoice, 3, func(a []any) any {
		sort := "select"
		if s, ok := a[0].(string); ok {
			sort = s
		} else {
			sort = a[0].(*z3.Sym).String()
		}
		li := a[1].(*z3.Cell)
		proc := a[2].(*z3.Closure)
		id, zid := newID()
		changed := func(s string) {
			li2 := &z3.Cell{Car: s, Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.SafeEvalWithInfo(li, z3.Nil, "%v\n"+fmt.Sprintf("IN choice %v callback", z3.Str(zid)))
		}
		s := make([]string, 0)
		for li != z3.Nil {
			s = append(s, li.Car.(string))
			li = li.CdrCell()
		}
		switch sort {
		case "select":
			putWithID(id, widget.NewSelect(s, changed))
		case "radio", "radio-group":
			putWithID(id, widget.NewRadioGroup(s, changed))
		default:
			panic(fmt.Sprintf(fnNewChoice+": the first argument must be one of '(select radio-group), given %v", sort))
		}
		return zid
	})

	fnSetSelectOptions := pre("set-select-options")
	// (set-select-options select string-list)
	interp.Def(fnSetSelectOptions, 2, func(a []any) any {
		sel := mustGet(fnSetSelectOptions, "GUI select ID", a, 0).(*widget.Select)
		li := a[1].(*z3.Cell)
		s := make([]string, 0)
		for li != z3.Nil {
			s = append(s, li.Car.(string))
			li = li.CdrCell()
		}
		sel.SetOptions(s)
		return z3.Void
	})

	// FORM

	fnNewForm := pre("new-form")
	// (new-form)
	interp.Def(fnNewForm, 0, func(a []any) any {
		return put(widget.NewForm())
	})

	fnAppendForm := pre("append-form")
	// (append-form <form> <string> <canvas-object>)
	interp.Def(fnAppendForm, 3, func(a []any) any {
		form := mustGet(fnAppendForm, "form", a, 0)
		obj := mustGet(fnAppendForm, "canvas-object", a, 2)
		form.(*widget.Form).Append(a[1].(string), obj.(fyne.CanvasObject))
		return z3.Void
	})

	// HYPERLINK

	fnNewHyperlink := pre("new-hyperlink")
	// (new-hyperlink label url) => int
	interp.Def(fnNewHyperlink, 2, func(a []any) any {
		if !cfg.HyperlinksAllowed {
			panic(fnNewHyperlink + ": hyperlinks are not permitted!")
		}
		url, err := url.Parse(a[1].(string))
		if err != nil {
			panic(fmt.Errorf(fnNewHyperlink+`: %w, given %v`, err, a[1].(string)))
		}
		if cfg.CheckHyperlinks != nil {
			url = cfg.CheckHyperlinks(url)
		}
		if url == nil {
			panic(fmt.Sprintf(fnNewHyperlink+`new-hyperlink:the link URL "%v" is not permitted!`, a[1].(string)))
		}
		return put(widget.NewHyperlink(a[0].(string), url))
	})

	// BUTTON

	fnNewButton := pre("new-button")
	// (new-button str proc) => int
	interp.Def(fnNewButton, 2, func(a []any) any {
		proc := a[1].(*z3.Closure)
		id, zid := newID()
		b := widget.NewButton(a[0].(string), func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.SafeEvalWithInfo(li, z3.Nil, "%v\n"+fmt.Sprintf("IN button %v callback", z3.Str(zid)))
		})
		putWithID(id, b)
		return zid
	})

	fnNewButtonWithIcon := pre("new-button-with-icon")
	interp.Def(fnNewButtonWithIcon, 3, func(a []any) any {
		icon := mustGet(fnNewButtonWithIcon, "GUI icon resource ID", a, 1).(fyne.Resource)
		proc := a[2].(*z3.Closure)
		id, zid := newID()
		b := widget.NewButtonWithIcon(a[0].(string), icon, func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.SafeEvalWithInfo(li, z3.Nil, "%v\n"+fmt.Sprintf("IN icon button %v callback", z3.Str(zid)))
		})
		putWithID(id, b)
		return zid
	})

	// LIST

	fnNewList := pre("new-list")
	// (new-list <len-proc> <prepare-proc> <update-proc>)
	interp.Def(fnNewList, 3, func(a []any) any {
		lproc := a[0].(*z3.Closure)
		pproc := a[1].(*z3.Closure)
		uproc := a[2].(*z3.Closure)
		id, zid := newID()
		lenCB := func() int {
			n, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: lproc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN list %v length callback", z3.Str(zid)))
			return int(z3.ToInt64("GUI list length callback", n))
		}
		prepCB := func() fyne.CanvasObject {
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: pproc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN list %v preparation callback", z3.Str(zid)))
			obj := mustGet1(fnNewList, "GUI canvas object ID (result from list preparation callback)", result)
			return obj.(fyne.CanvasObject)
		}
		updCB := func(i widget.ListItemID, o fyne.CanvasObject) {
			n := int(i)
			if id, ok := getID(o); ok {
				interp.SafeEvalWithInfo(&z3.Cell{Car: uproc, Cdr: &z3.Cell{Car: id, Cdr: &z3.Cell{Car: goarith.AsNumber(n), Cdr: z3.Nil}}}, z3.Nil,
					"%v\n"+fmt.Sprintf("IN list %v update callback", z3.Str(zid)))
			}
		}
		putWithID(id, widget.NewList(lenCB, prepCB, updCB))
		return zid
	})

	// TABLE

	fnNewTable := pre("new-table")
	// (new-table <len-proc> <prepare-proc> <update-proc>)
	interp.Def(fnNewTable, 3, func(a []any) any {
		lproc := a[0].(*z3.Closure)
		pproc := a[1].(*z3.Closure)
		uproc := a[2].(*z3.Closure)
		id, zid := newID()
		lenCB := func() (int, int) {
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: lproc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN table %v length callback", z3.Str(zid)))
			c := result.(*z3.Cell)
			a := z3.ToInt64("GUI table length callback return element #1", c.Car)
			c = c.CdrCell()
			b := z3.ToInt64("GUI table length callback return element #2", c.Car)
			return int(a), int(b)
		}
		prepCB := func() fyne.CanvasObject {
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: pproc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN table %v preparation callback", z3.Str(zid)))
			obj := mustGet1(fnNewTable, "GUI canvas object ID (result from table preparation callback)", result)
			return obj.(fyne.CanvasObject)
		}
		updCB := func(id widget.TableCellID, o fyne.CanvasObject) {
			row := id.Row
			col := id.Col
			if id, ok := getID(o); ok {
				interp.SafeEvalWithInfo(&z3.Cell{Car: uproc, Cdr: &z3.Cell{Car: goarith.AsNumber(id), Cdr: &z3.Cell{Car: goarith.AsNumber(row), Cdr: &z3.Cell{Car: goarith.AsNumber(col), Cdr: z3.Nil}}}}, z3.Nil,
					"%v\n"+fmt.Sprintf("IN table %v update callback", z3.Str(zid)))
			}
		}
		putWithID(id, widget.NewTable(lenCB, prepCB, updCB))
		return zid
	})

	// TREE

	fnNewTree := pre("new-tree")
	// (new-tree <child-uid-proc> <is-branch-proc> <create-node-proc> <update-note-proc)
	interp.Def(fnNewTree, 4, func(a []any) any {
		proc1 := a[0].(*z3.Closure)
		proc2 := a[1].(*z3.Closure)
		proc3 := a[2].(*z3.Closure)
		proc4 := a[3].(*z3.Closure)
		id, zid := newID()
		fn1 := func(id widget.TreeNodeID) []widget.TreeNodeID {
			var s string = id
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: proc1, Cdr: &z3.Cell{Car: s, Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN tree %v child UID callback", z3.Str(zid)))
			li := result.(*z3.Cell)
			arr := make([]widget.TreeNodeID, 0)
			for li != z3.Nil {
				arr = append(arr, widget.TreeNodeID(li.Car.(string)))
				li = li.CdrCell()
			}
			return arr
		}
		fn2 := func(id widget.TreeNodeID) bool {
			var s string = id
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: proc2, Cdr: &z3.Cell{Car: s, Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN tree %v is-branch callback", z3.Str(zid)))
			return z3.ToBool(result)
		}
		fn3 := func(branch bool) fyne.CanvasObject {
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: proc3, Cdr: &z3.Cell{Car: z3.AsLispBool(branch), Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN tree %v creation callback", z3.Str(zid)))
			obj := mustGet1(fnNewTree, "GUI tree creation callback canvas object ID", result)
			return obj.(fyne.CanvasObject)
		}
		fn4 := func(id widget.TreeNodeID, branch bool, o fyne.CanvasObject) {
			var s string = id
			objID, _ := getID(o)
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc4, Cdr: &z3.Cell{Car: s, Cdr: &z3.Cell{Car: z3.AsLispBool(branch),
				Cdr: &z3.Cell{Car: objID, Cdr: z3.Nil}}}}, z3.Nil, "%v\n"+fmt.Sprintf("IN tree %v update callback", z3.Str(zid)))
		}
		putWithID(id, widget.NewTree(fn1, fn2, fn3, fn4))
		return zid
	})

	// MENU

	fnNewMenuItem := pre("new-menu-item")
	// (new-menu-item <str> <proc> [<selector>...]) => menu item ID
	interp.Def(fnNewMenuItem, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		label := li.Car.(string)
		li = li.CdrCell()
		proc := li.Car.(*z3.Closure)
		li = li.CdrCell()
		id, zid := newID()
		item := fyne.NewMenuItem(label, func() {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN menu-item %v callback", z3.Str(zid)))
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
				panic(fmt.Sprintf(fnNewMenuItem+": expected a symbol in '(is-quit is-separator disabled checked) but given %v", z3.Str(li.Car)))
			}
			li = li.CdrCell()
		}
		putWithID(id, item)
		return zid
	})

	fnSetMenuItemChecked := pre("set-menu-item-checked")
	// (set-menu-item-checked <item> <bool>)
	interp.Def(fnSetMenuItemChecked, 2, func(a []any) any {
		item := mustGet(fnSetMenuItemChecked, "GUI menu item ID", a, 0).(*fyne.MenuItem)
		b := z3.ToBool(a[1])
		item.Checked = b
		return z3.Void
	})

	fnMenuItemChecked := pre("menu-item-checked?")
	// (menu-item-checked? <item>) => bool
	interp.Def(fnMenuItemChecked, 1, func(a []any) any {
		item := mustGet(fnMenuItemChecked, "GUI menu item ID", a, 0).(*fyne.MenuItem)
		return z3.AsLispBool(item.Checked)
	})

	fnSetMenuItemDisabled := pre("set-menu-item-disabled")
	// (set-menu-item-disabled <item> <bool>)
	interp.Def(fnSetMenuItemDisabled, 2, func(a []any) any {
		item := mustGet(fnSetMenuItemDisabled, "GUI menu item ID", a, 0).(*fyne.MenuItem)
		b := z3.ToBool(a[1])
		item.Disabled = b
		return z3.Void
	})

	fnMenuItemDisabled := pre("menu-item-disabled?")
	// (menu-item-disabled? <item>) => bool
	interp.Def(fnMenuItemDisabled, 1, func(a []any) any {
		item := mustGet(fnMenuItemDisabled, "GUI menu item ID", a, 0).(*fyne.MenuItem)
		return z3.AsLispBool(item.Disabled)
	})

	fnGetMenuItemLabel := pre("get-menu-item-label")
	// (get-menu-item-label <item>) => str
	interp.Def(fnGetMenuItemLabel, 1, func(a []any) any {
		item := mustGet(fnGetMenuItemLabel, "GUI menu item ID", a, 0).(*fyne.MenuItem)
		return item.Label
	})

	fnSetMenuItemLabel := pre("set-menu-item-label")
	// (set-menu-item-label <item> <str>)
	interp.Def(fnSetMenuItemLabel, 2, func(a []any) any {
		item := mustGet(fnSetMenuItemLabel, "GUI menu item ID", a, 0).(*fyne.MenuItem)
		item.Label = a[1].(string)
		return z3.Void
	})

	fnNewMenuItemSeparator := pre("new-menu-item-separator")
	// (new-menu-item-separator) => menu item ID
	interp.Def(fnNewMenuItemSeparator, 0, func(a []any) any {
		return put(fyne.NewMenuItemSeparator())
	})

	fnNewMenuAlt := pre("new-menu*")
	// (new-menu* <str> [<item>...]) => menu* ID
	interp.Def(fnNewMenuAlt, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		label := li.Car.(string)
		li = li.CdrCell()
		items := make([]*fyne.MenuItem, 0)
		for li != z3.Nil {
			item := mustGet1(fnNewMenuAlt, "GUI menu item ID", li.Car).(*fyne.MenuItem)
			items = append(items, item)
			li = li.CdrCell()
		}
		return put(fyne.NewMenu(label, items...))
	})

	fnRefreshMenuAlt := pre("refresh-menu*")
	// (refresh-menu* <menu>)
	interp.Def(fnRefreshMenuAlt, 1, func(a []any) any {
		menu := mustGet(fnRefreshMenuAlt, "GUI menu item ID", a, 0).(*fyne.Menu)
		menu.Refresh()
		return z3.Void
	})

	fnNewMenu := pre("new-menu")
	// (new-menu <menu>) => menu ID
	interp.Def(fnNewMenu, 1, func(a []any) any {
		m := mustGet(fnNewMenu, "GUI menu* ID", a, 0).(*fyne.Menu)
		return put(widget.NewMenu(m))
	})

	fnActivateMenuLastSubmenu := pre("activate-menu-last-submenu")
	// (activate-menu-last-submenu <menu>) => bool
	interp.Def(fnActivateMenuLastSubmenu, 1, func(a []any) any {
		menu := mustGet(fnActivateMenuLastSubmenu, "GUI menu ID", a, 0).(*widget.Menu)
		return z3.AsLispBool(menu.ActivateLastSubmenu())
	})

	fnActivateMenuNext := pre("activate-menu-next")
	// (activate-menu-next <menu>)
	interp.Def(fnActivateMenuNext, 1, func(a []any) any {
		menu := mustGet(fnActivateMenuNext, "GUI menu ID", a, 0).(*widget.Menu)
		menu.ActivateNext()
		return z3.Void
	})

	fnActivateMenuPrevious := pre("activate-menu-previous")
	// (activate-menu-previous <menu>)
	interp.Def(fnActivateMenuPrevious, 1, func(a []any) any {
		menu := mustGet(fnActivateMenuPrevious, "GUI menu ID", a, 0).(*widget.Menu)
		menu.ActivatePrevious()
		return z3.Void
	})

	fnDeactivateMenuChild := pre("deactivate-menu-child")
	// (deactivate-menu-child <menu>)
	interp.Def(fnDeactivateMenuChild, 1, func(a []any) any {
		menu := mustGet(fnDeactivateMenuChild, "GUI menu ID", a, 0).(*widget.Menu)
		menu.DeactivateChild()
		return z3.Void
	})

	fnDeactivateMenuLastSubmenu := pre("deactivate-menu-last-submenu")
	// (deactivate-menu-last-submenu <menu>)
	interp.Def(fnDeactivateMenuLastSubmenu, 1, func(a []any) any {
		menu := mustGet(fnDeactivateMenuLastSubmenu, "GUI menu ID", a, 0).(*widget.Menu)
		menu.DeactivateLastSubmenu()
		return z3.Void
	})

	fnTriggerMenuLast := pre("trigger-menu-last")
	// (trigger-menu-last <menu>)
	interp.Def(fnTriggerMenuLast, 1, func(a []any) any {
		menu := mustGet(fnTriggerMenuLast, "GUI menu ID", a, 0).(*widget.Menu)
		menu.TriggerLast()
		return z3.Void
	})

	fnNewMainMenu := pre("new-main-menu")
	// (new-main-menu <menu> ...) => main menu ID
	interp.Def(fnNewMainMenu, -1, func(a []any) any {
		items := make([]*fyne.Menu, 0)
		li := a[0].(*z3.Cell)
		for li != z3.Nil {
			menu := li.Car.(*fyne.Menu)
			items = append(items, menu)
			li = li.CdrCell()
		}
		return put(fyne.NewMainMenu(items...))
	})

	fnRefreshMainMenu := pre("refresh-main-menu")
	// (refresh-main-menu <menu>)
	interp.Def(fnRefreshMainMenu, 1, func(a []any) any {
		menu := mustGet(fnRefreshMainMenu, "GUI main menu ID", a, 0).(*fyne.MainMenu)
		menu.Refresh()
		return z3.Void
	})

	// IMAGE

	fnNewImageFromResource := pre("new-image-from-resource")
	// (new-image-from-resource <resource>)
	interp.Def(fnNewImageFromResource, 1, func(a []any) any {
		res := mustGet(fnNewImageFromResource, "GUI resource ID", a, 0)
		return put(canvas.NewImageFromResource(res.(fyne.Resource)))
	})

	fnNewImageFromFile := pre("new-image-from-file")
	// (new-image-from-file <path-string>)
	interp.Def(fnNewImageFromFile, 1, func(a []any) any {
		return put(canvas.NewImageFromFile(a[0].(string)))
	})

	// DRAWING

	// color helpers

	fnRGBA := pre("nrgba")
	// (nrgba r g b a) => NRGBA color
	interp.Def(fnRGBA, 4, func(a []any) any {
		r := z3.ToUInt8(a[0])
		g := z3.ToUInt8(a[1])
		b := z3.ToUInt8(a[2])
		alpha := z3.ToUInt8(a[3])
		return put(color.NRGBA{R: r, G: g, B: b, A: alpha})
	})

	fnNRGBA := pre("nrgba64")
	// (nrgba64 r g b a) => NRGBA64 color
	interp.Def(fnNRGBA, 4, func(a []any) any {
		r := z3.ToUInt16(a[0])
		g := z3.ToUInt16(a[1])
		b := z3.ToUInt16(a[2])
		alpha := z3.ToUInt16(a[3])
		return put(color.NRGBA64{R: r, G: g, B: b, A: alpha})
	})

	fnNewRectangle := pre("new-rectangle")
	// (new-rectangle <fill-color> [<width> <height>] [<position>] [<stroke-color>] [<stroke-width>] [<corner-radius>])
	interp.Def(fnNewRectangle, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		fillColor := mustGet1(fnNewRectangle, "GUI nrgba color ID", li.Car).(color.Color)
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
		pos, ok := MustGetPosition(fnNewRectangle, 3, li.Car)
		if ok {
			rect.Move(pos)
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(rect)
		}
		strokeColor := mustGet1(fnNewRectangle, "GUI nrgba color ID", li.Car).(color.Color)
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

	fnSetRectangleMinSize := pre("set-rectangle-min-size")
	interp.Def(fnSetRectangleMinSize, 3, func(a []any) any {
		r := mustGet1(fnSetRectangleMinSize, "GUI rectangle ID", a[0])
		w := z3.ToFloat64(a[1])
		h := z3.ToFloat64(a[2])
		r.(*canvas.Rectangle).SetMinSize(fyne.NewSize(float32(w), float32(h)))
		return z3.Void
	})

	fnNewCircle := pre("new-circle")
	// (new-circle <fill-color> [<pos1>] [<pos2>] [<stroke-color>] [<stroke-width>])
	interp.Def(fnNewCircle, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		fillColor := mustGet1(fnNewCircle, "GUI nrgba color ID", li.Car).(color.Color)
		circle := canvas.NewCircle(fillColor)
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		pos1, ok := MustGetPosition(fnNewCircle, 0, li.Car)
		if ok {
			circle.Position1 = pos1
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		pos2, ok := MustGetPosition(fnNewCircle, 1, li.Car)
		if ok {
			circle.Position2 = pos2
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		strokeColor := mustGet1(fnNewCircle, "GUI nrgba color ID", li.Car).(color.Color)
		circle.StrokeColor = strokeColor
		li = li.CdrCell()
		if li == z3.Nil {
			return put(circle)
		}
		circle.StrokeWidth = float32(z3.ToFloat64(li.Car))
		return put(circle)
	})

	fnNewLine := pre("new-line")
	// (new-line <fill-color> [<pos1>] [<pos2>] [<stroke-color>] [<stroke-width>])
	interp.Def(fnNewLine, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		fillColor := mustGet1(fnNewLine, "GUI nrgba color ID", li.Car).(color.Color)
		line := canvas.NewLine(fillColor)
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		pos1, ok := MustGetPosition(fnNewLine, 0, li.Car)
		if ok {
			line.Position1 = pos1
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		pos2, ok := MustGetPosition(fnNewLine, 1, li.Car)
		if ok {
			line.Position2 = pos2
		}
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		strokeColor := mustGet1(fnNewLine, "GUI nrgba color ID", li.Car).(color.Color)
		line.StrokeColor = strokeColor
		li = li.CdrCell()
		if li == z3.Nil {
			return put(line)
		}
		line.StrokeWidth = float32(z3.ToFloat64(li.Car))
		return put(line)
	})

	fnNewText := pre("new-text")
	interp.Def(fnNewText, 2, func(a []any) any {
		s := a[0].(string)
		color := mustGet1(fnNewText, "GUI nrgba color ID", a[1]).(color.Color)
		return put(canvas.NewText(s, color))
	})

	fnSetTextAlignment := pre("set-text-alignment")
	interp.Def(fnSetTextAlignment, 2, func(a []any) any {
		text := mustGet1(fnSetTextAlignment, "GUI text ID", a[0])
		align, ok := MustGetTextAlign(fnSetTextAlignment, 1, a[1])
		if ok {
			text.(*canvas.Text).Alignment = align
		}
		return z3.Void
	})

	fnSetTextSize := pre("set-text-size")
	interp.Def(fnSetTextSize, 2, func(a []any) any {
		text := mustGet1(fnSetTextSize, "GUI text ID", a[0])
		text.(*canvas.Text).TextSize = float32(z3.ToFloat64(a[1]))
		return z3.Void
	})

	fnSetTextStyle := pre("set-text-style")
	interp.Def(fnSetTextStyle, 2, func(a []any) any {
		text := mustGet1(fnSetTextStyle, "GUI text ID", a[0]).(*canvas.Text)
		style := MustGetTextStyle(fnSetTextStyle, 1, a[1])
		text.TextStyle = style
		return z3.Void
	})

	fnNewRasterWithPixels := pre("new-raster-with-pixels")
	// (new-raster-with-pixels <pixel-proc>) where <pixel-proc> takes x, y, w, h and returns
	// a color list (NOT an nrgba64 color, for performance reasons this is created at the Go side).
	interp.Def(fnNewRasterWithPixels, 1, func(a []any) any {
		proc := a[0].(*z3.Closure)
		id, zid := newID()
		raster := canvas.NewRasterWithPixels(func(x, y, w, h int) color.Color {
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: goarith.AsNumber(x), Cdr: &z3.Cell{Car: goarith.AsNumber(y),
				Cdr: &z3.Cell{Car: goarith.AsNumber(w), Cdr: &z3.Cell{Car: goarith.AsNumber(h), Cdr: z3.Nil}}}}}
			result, _ := interp.SafeEvalWithInfo(li, z3.Nil, "%v\n"+fmt.Sprintf("IN raster %v callback", z3.Str(zid)))
			li = result.(*z3.Cell)
			return ListToColor(li)
		})
		putWithID(id, raster)
		return zid
	})

	// DIALOG

	fnShowColorPicker := pre("show-color-picker")
	// (show-color-picker title message proc win)
	interp.Def(fnShowColorPicker, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		title := li.Car.(string)
		li = li.CdrCell()
		msg := li.Car.(string)
		li = li.CdrCell()
		proc := li.Car.(*z3.Closure)
		li = li.CdrCell()
		win := mainWin
		if li != nil {
			w, ok := get(li.Car)
			if !ok {
				panic(fmt.Sprintf(fnShowColorPicker+": no window found for %v", z3.Str(li.Car)))
			}
			win = w.(fyne.Window)
		}
		dialog.ShowColorPicker(title, msg, func(c color.Color) {
			li2 := ColorToList(c)
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: z3.QqQuote(li2), Cdr: z3.Nil}}, z3.Nil,
				"%v\n IN show-color-picker callback")
		}, win)
		return z3.Void
	})

	fnShowConfirm := pre("show-confirm")
	// (show-confirm title message proc win)
	interp.Def(fnShowConfirm, 4, func(a []any) any {
		proc := a[2].(*z3.Closure)
		win := mustGet(fnShowConfirm, "GUI window ID", a, 3).(fyne.Window)
		dialog.ShowConfirm(a[0].(string), a[1].(string), func(confirm bool) {
			b := z3.AsLispBool(confirm)
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: b, Cdr: z3.Nil}}, z3.Nil,
				"%v\n IN "+fnShowConfirm+" callback")
		}, win)
		return z3.Void
	})

	fnShowCustom := pre("show-custom")
	// (show-custom title dismiss content win)
	interp.Def(fnShowCustom, 4, func(a []any) any {
		content := mustGet(fnShowCustom, "GUI canvas object ID", a, 2).(fyne.CanvasObject)
		win := mustGet(fnShowCustom, "GUI window ID", a, 3).(fyne.Window)
		dialog.ShowCustom(a[0].(string), a[1].(string), content, win)
		return z3.Void
	})

	fnShowCustomConfirm := pre("show-custom-confirm")
	// (show-custom-confirm title confirm dismiss content callback win)
	interp.Def(fnShowCustomConfirm, 6, func(a []any) any {
		content := mustGet(fnShowCustomConfirm, "GUI canvas object ID", a, 3).(fyne.CanvasObject)
		proc := a[4].(*z3.Closure)
		win := mustGet(fnShowCustomConfirm, "GUI window ID", a, 5).(fyne.Window)
		dialog.ShowCustomConfirm(a[0].(string), a[1].(string), a[2].(string), content,
			func(confirm bool) {
				b := z3.AsLispBool(confirm)
				interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: b, Cdr: z3.Nil}}, z3.Nil,
					"%v\n IN "+fnShowCustomConfirm+" callback")
			}, win)
		return z3.Void
	})

	fnShowCustomWithoutButtons := pre("show-custom-without-buttons")
	// (show-custom-without-buttons title content win)
	interp.Def(fnShowCustomWithoutButtons, 3, func(a []any) any {
		content := mustGet(fnShowCustomWithoutButtons, "GUI canvas object ID", a, 1).(fyne.CanvasObject)
		win := mustGet(fnShowCustomWithoutButtons, "GUI window ID", a, 2).(fyne.Window)
		dialog.ShowCustomWithoutButtons(a[0].(string), content, win)
		return z3.Void
	})

	fnShowInformation := pre("show-information")
	// (show-information title message win)
	interp.Def(fnShowInformation, 3, func(a []any) any {
		win := mustGet(fnShowCustomWithoutButtons, "GUI window ID", a, 2).(fyne.Window)
		dialog.ShowInformation(a[0].(string), a[1].(string), win)
		return z3.Void
	})

	fnShowForm := pre("show-form")
	// (show-form title confirm dismiss li proc win)
	interp.Def(fnShowForm, 6, func(a []any) any {
		forms := a[3].(*z3.Cell)
		items := make([]*widget.FormItem, 0)
		for forms != z3.Nil {
			item := mustGet1(fnShowForm, "GUI form item ID", forms.Car).(*widget.FormItem)
			items = append(items, item)
			forms = forms.CdrCell()
		}
		win := mustGet(fnShowForm, "GUI window ID", a, 5).(fyne.Window)
		proc := a[4].(*z3.Closure)
		dialog.ShowForm(a[0].(string), a[1].(string), a[2].(string), items, func(validated bool) {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: z3.AsLispBool(validated), Cdr: z3.Nil}}, z3.Nil,
				"%v\n IN "+fnShowForm+" callback")
		}, win)
		return z3.Void
	})

	fnNewFormItem := pre("new-form-item")
	// (new-form-item text widget hint) => int
	interp.Def(fnNewFormItem, 3, func(a []any) any {
		text := a[0].(string)
		content := mustGet(fnShowCustomWithoutButtons, "GUI canvas object ID", a, 1).(fyne.CanvasObject)
		hint := a[2].(string)
		item := &widget.FormItem{
			Text:     text,
			Widget:   content,
			HintText: hint,
		}
		return put(item)
	})

	// KEYBOARD

	// CANVAS

	fnAddCanvasShortcut := pre("add-canvas-shortcut")
	// (add-canvas-shortcut canvas shortcut proc)
	interp.Def(fnAddCanvasShortcut, 3, func(a []any) any {
		canvas := mustGet(fnAddCanvasShortcut, "GUI canvas ID", a, 0).(fyne.Canvas)
		key, modifier := MustGetShortcut(fnAddCanvasShortcut, a[1].(*z3.Cell))
		proc := a[2].(*z3.Closure)
		shortcut := &desktop.CustomShortcut{KeyName: key, Modifier: modifier}
		canvas.AddShortcut(shortcut, func(sc fyne.Shortcut) {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN canvas %v shortcut handler", z3.Str(a[0])))
		})
		return z3.Void
	})

	fnRemoveCanvasShortcut := pre("remove-canvas-shortcut")
	// (remove-canvas-shortcut canvas shortcut)
	interp.Def(fnRemoveCanvasShortcut, 2, func(a []any) any {
		canvas := mustGet(fnRemoveCanvasShortcut, "GUI canvas ID", a, 0).(fyne.Canvas)
		key, modifier := MustGetShortcut(fnRemoveCanvasShortcut, a[1].(*z3.Cell))
		shortcut := &desktop.CustomShortcut{KeyName: key, Modifier: modifier}
		canvas.RemoveShortcut(shortcut)
		return z3.Void
	})

	fnSetCanvasOnTypedKey := pre("set-canvas-on-typed-key")
	// (set-canvas-on-typed-key canvas proc)
	interp.Def(fnSetCanvasOnTypedKey, 2, func(a []any) any {
		canvas := mustGet(fnSetCanvasOnTypedKey, "GUI canvas ID", a, 0).(fyne.Canvas)
		proc := a[1].(*z3.Closure)
		canvas.SetOnTypedKey(func(evt *fyne.KeyEvent) {
			qq := z3.QqQuote(KeyNameToSymbol(evt.Name))
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: qq, Cdr: &z3.Cell{Car: goarith.AsNumber(evt.Physical.ScanCode), Cdr: z3.Nil}}}
			interp.SafeEvalWithInfo(li, z3.Nil,
				"%v\n"+fmt.Sprintf("IN canvas %v on-typed-key handler", a[0]))
		})
		return z3.Void
	})

	fnSetCanvasOnTypedRune := pre("set-canvas-on-typed-rune")
	// (set-canvas-on-typed-rune canvas proc)
	interp.Def(fnSetCanvasOnTypedRune, 2, func(a []any) any {
		canvas := mustGet(fnSetCanvasOnTypedRune, "GUI canvas ID", a, 0).(fyne.Canvas)
		proc := a[1].(*z3.Closure)
		canvas.SetOnTypedRune(func(r rune) {
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: string(r), Cdr: z3.Nil}}
			interp.SafeEvalWithInfo(li, z3.Nil,
				"%v\n"+fmt.Sprintf("IN canvas %v on-typed-rune handler", a[0]))
		})
		return z3.Void
	})

	fnFocusCanvasObject := pre("focus-canvas-object")
	// (focus-canvas-object canvas object)
	interp.Def(fnFocusCanvasObject, 2, func(a []any) any {
		canvas := mustGet(fnFocusCanvasObject, "GUI canvas ID", a, 0).(fyne.Canvas)
		obj, ok := mustGet(fnFocusCanvasObject, "GUI focusable canvas object ID", a, 1).(fyne.Focusable)
		if !ok {
			panic(fmt.Sprintf("%v: expected a focusable canvas object as second argument, but the given canvas object cannot take focus: %v", pre("focus-canvas-object"), a[1]))
		}
		canvas.Focus(obj)
		return z3.Void
	})

	fnFocusNextCanvasObject := pre("focus-next-canvas-object")
	// (focus-next-canvas-object canvas)
	interp.Def(fnFocusNextCanvasObject, 1, func(a []any) any {
		canvas := mustGet(fnFocusNextCanvasObject, "GUI canvas ID", a, 0).(fyne.Canvas)
		canvas.FocusNext()
		return z3.Void
	})

	fnUnfocusCanvasObjects := pre("unfocus-canvas-objects")
	// (unfocus-canvas-objects canvas)
	interp.Def(fnUnfocusCanvasObjects, 1, func(a []any) any {
		canvas := mustGet(fnUnfocusCanvasObjects, "GUI canvas ID", a, 0).(fyne.Canvas)
		canvas.Unfocus()
		return z3.Void
	})

	fnFocusPreviousCanvasObject := pre("focus-previous-canvas-object")
	// (focus-previous-canvas-object canvas)
	interp.Def(fnFocusPreviousCanvasObject, 1, func(a []any) any {
		canvas := mustGet(fnFocusPreviousCanvasObject, "GUI canvas ID", a, 0).(fyne.Canvas)
		canvas.FocusPrevious()
		return z3.Void
	})

	fnGetFocusedCanvasObject := pre("get-focused-canvas-object")
	// (get-focused-canvas-object canvas) => int or nil
	interp.Def(fnGetFocusedCanvasObject, 1, func(a []any) any {
		canvas := mustGet(fnGetFocusedCanvasObject, "GUI canvas ID", a, 0).(fyne.Canvas)
		focused := canvas.Focused()
		if focused == nil {
			return z3.Nil
		}
		if obj, ok := focused.(fyne.Focusable); ok {
			id, found := getID(obj)
			if !found {
				return z3.Nil
			}
			return id
		}
		return z3.Nil
	})

	// CANVAS OBJECT (polymorphic methods)

	fnDisableObject := pre("disable-object")
	// (disable-object <obj>)
	interp.Def(fnDisableObject, 2, func(a []any) any {
		obj := mustGet(fnDisableObject, "GUI disableable object ID", a, 0)
		obj.(fyne.Disableable).Disable()
		return z3.Void
	})

	fnEnableObject := pre("enable-object")
	// (enable-object <obj>)
	interp.Def(fnEnableObject, 2, func(a []any) any {
		obj := mustGet(fnEnableObject, "GUI disableable object ID", a, 0)
		obj.(fyne.Disableable).Enable()
		return z3.Void
	})

	fnHideObject := pre("hide-object")
	// (hide-object <obj>)
	interp.Def(fnHideObject, 2, func(a []any) any {
		obj := mustGet(fnHideObject, "GUI widget ID", a, 0)
		obj.(fyne.Widget).Hide()
		return z3.Void
	})

	fnShowObject := pre("show-object")
	// (show-object <obj>)
	interp.Def(fnShowObject, 2, func(a []any) any {
		obj := mustGet(fnShowObject, "GUI widget ID", a, 0)
		obj.(fyne.Widget).Show()
		return z3.Void
	})

	fnObjectDisabled := pre("object-disabled?")
	// (object-disabled? <obj>) => bool
	interp.Def(fnObjectDisabled, 2, func(a []any) any {
		obj := mustGet(fnObjectDisabled, "GUI disableable object ID", a, 0)
		return z3.AsLispBool(obj.(fyne.Disableable).Disabled())
	})

	fnMoveObject := pre("move-object")
	// (move-object <obj> <pos>) attempts to move a GUI object (polymorphic)
	interp.Def(fnMoveObject, 2, func(a []any) any {
		obj := mustGet(fnMoveObject, "GUI canvas object ID", a, 0)
		pos, ok := MustGetPosition(fnMoveObject, 1, a[1])
		if !ok {
			return z3.Void
		}
		obj.(fyne.CanvasObject).Move(pos)
		return z3.Void
	})

	fnResizeObject := pre("resize-object")
	// (resize-object <obj> <w> <h>) attempts to resize a GUI object (polymorphic)
	interp.Def(fnResizeObject, 3, func(a []any) any {
		obj := mustGet(fnResizeObject, "GUI canvas object ID", a, 0)
		w := float32(z3.ToFloat64(a[1]))
		h := float32(z3.ToFloat64(a[2]))
		obj.(fyne.CanvasObject).Resize(fyne.NewSize(w, h))
		return z3.Void
	})

	fnGetObjectSize := pre("get-object-size")
	// (get-object-size <obj>) => li
	interp.Def(fnGetObjectSize, 1, func(a []any) any {
		obj := mustGet(fnGetObjectSize, "GUI canvas object ID", a, 0)
		size := obj.(fyne.CanvasObject).Size()
		return &z3.Cell{Car: goarith.AsNumber(float64(size.Width)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(size.Height)), Cdr: z3.Nil}}
	})

	fnGetObjectMinSize := pre("get-object-min-size")
	// (get-object-min-size <obj>) => li
	interp.Def(fnGetObjectMinSize, 1, func(a []any) any {
		obj := mustGet(fnGetObjectMinSize, "GUI canvas object ID", a, 0)
		size := obj.(fyne.CanvasObject).MinSize()
		return &z3.Cell{Car: goarith.AsNumber(float64(size.Width)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(size.Height)), Cdr: z3.Nil}}
	})

	fnGetObjectPosition := pre("get-object-position")
	// (get-object-position <obj>) => li
	interp.Def(fnGetObjectPosition, 1, func(a []any) any {
		obj := mustGet(fnGetObjectPosition, "GUI canvas object ID", a, 0)
		pos := obj.(fyne.CanvasObject).Position()
		return &z3.Cell{Car: goarith.AsNumber(float64(pos.X)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(pos.Y)), Cdr: z3.Nil}}
	})

	fnObjectVisible := pre("object-visible?")
	// (object-visible? <obj>) => bool
	interp.Def(fnObjectVisible, 1, func(a []any) any {
		obj := mustGet(fnObjectVisible, "GUI canvas object ID", a, 0)
		return z3.AsLispBool(obj.(fyne.CanvasObject).Visible())
	})

	fnRefreshObject := pre("refresh-object")
	// (refresh-object <obj>)
	interp.Def(fnRefreshObject, 1, func(a []any) any {
		obj := mustGet(fnRefreshObject, "GUI canvas object ID", a, 0)
		obj.(fyne.CanvasObject).Refresh()
		return z3.Void
	})

	// PROGRESSBAR

	fnNewProgressBar := pre("new-progress-bar")
	// (new-progress-bar)
	interp.Def(fnNewProgressBar, 0, func(a []any) any {
		return put(widget.NewProgressBar())
	})

	fnNewInfiniteProgressBar := pre("new-infinite-progress-bar")
	// (new-infinite-progress-bar)
	interp.Def(fnNewInfiniteProgressBar, 0, func(a []any) any {
		return put(widget.NewProgressBarInfinite())
	})

	fnSetProgressBar := pre("set-progress-bar")
	// (set-progress-bar <bar> <value>|[<selector> <value>])
	interp.Def(fnSetProgressBar, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		bar := mustGet1(fnSetProgressBar, "GUI progress-bar ID", li.Car).(*widget.ProgressBar)
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
					result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: id, Cdr: z3.Nil}}, z3.Nil,
						"%v\n"+fmt.Sprintf("IN progress-bar %v formatter callback", z3.Str(id)))
					if s, ok := result.(string); ok {
						return s
					}
					panic(fmt.Sprintf(fnSetProgressBar+": formatter callback is expected to return a string but it returned %v", z3.Str(result)))
				}
				bar.TextFormatter = fn
			default:
				panic(fmt.Sprintf(fnSetProgressBar+": expected selector in '(value min max formatter), given %v", sym.String()))
			}
			return z3.Void
		}
		bar.SetValue(z3.ToFloat64(li.Car))
		return z3.Void
	})

	fnGetProgressBarValue := pre("get-progress-bar-value")
	// (get-progress-bar-value <bar>)
	interp.Def(fnGetProgressBarValue, 1, func(a []any) any {
		bar := mustGet(fnGetProgressBarValue, "GUI progress-bar ID", a, 0)
		n := bar.(*widget.ProgressBar).Value
		return goarith.AsNumber(n)
	})

	// SLIDER

	fnNewSlider := pre("new-slider")
	// (new-slider <min> <max> <change-cb>)
	interp.Def(fnNewSlider, 3, func(a []any) any {
		proc := a[2].(*z3.Closure)
		id, zid := newID()
		fn := func(v float64) {
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: goarith.AsNumber(v), Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN slider %v callback", z3.Str(zid)))
		}
		slider := widget.NewSlider(z3.ToFloat64(a[0]), z3.ToFloat64(a[1]))
		slider.OnChanged = fn
		putWithID(id, slider)
		return zid
	})

	fnSetSliderValue := pre("set-slider-value")
	// (set-slider-value <slider> <value>)
	interp.Def(fnSetSliderValue, 2, func(a []any) any {
		slider := mustGet(fnSetSliderValue, "GUI slider ID", a, 0).(*widget.Slider)
		slider.SetValue(z3.ToFloat64(a[1]))
		return z3.Void
	})

	// ICON

	fnNewIcon := pre("new-icon")
	// (new-icon <resource>)
	interp.Def(fnNewIcon, 1, func(a []any) any {
		res := mustGet(fnNewIcon, "GUI resource ID", a, 0)
		return put(widget.NewIcon(res.(fyne.Resource)))
	})

	// MISC

	fnCreateLoremIpsum := pre("create-lorem-ipsum")
	interp.Def(fnCreateLoremIpsum, 3, func(a []any) any {
		sym := a[0].(*z3.Sym)
		nmin := z3.ToInt(fnCreateLoremIpsum, a[1])
		nmax := z3.ToInt(fnCreateLoremIpsum, a[2])
		switch sym {
		case LoremWord:
			return lorem.Word(nmin, nmax)
		case LoremSentence:
			return lorem.Sentence(nmin, nmax)
		case LoremParagraph:
			return lorem.Paragraph(nmin, nmax)
		default:
			panic(fmt.Sprintf("%v: expected a symbol in '(word sentence paragraph), given %v", pre("create-lorem-ipsum"),
				z3.Str(sym)))
		}
	})

	fnForgetGUIObject := pre("forget-gui-object")
	// (forget-gui-object <id>) clears any internal association with the given GUI object
	// but does not destroy resources associated with it. WARN: Internal use only, use with care!
	interp.Def(fnForgetGUIObject, 1, func(a []any) any {
		clear(a[0])
		return z3.Void
	})

	fnCloseGUI := pre("close-gui")
	// (close-gui) closes the GUI, none of its elements can be used again and the application
	// must shut down all GUI activity. Open windows are closed.
	interp.Def(fnCloseGUI, 0, func(a []any) any {
		CloseGUI()
		return z3.Void
	})

	fnGetClipboardContent := pre("get-clipboard-content")
	interp.Def(fnGetClipboardContent, 0, func(a []any) any {
		if !config.ClipboardGetAllowed {
			panic("getting clipboard content is prohibited by security policy!")
		}
		s := mainWin.Clipboard().Content()
		return s
	})

	fnSetClipboardContent := pre("set-clipboard-content")
	interp.Def(fnSetClipboardContent, 1, func(a []any) any {
		if !config.ClipboardSetAllowed {
			panic("setting clipboard content is prohibited by security policy!")
		}
		mainWin.Clipboard().SetContent(a[0].(string))
		return z3.Void
	})

	fnGetDeviceInfo := pre("get-device-info")
	interp.Def(fnGetDeviceInfo, 0, func(a []any) any {
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

	fnNewSpacer := pre("new-spacer")
	interp.Def(fnNewSpacer, 0, func(a []any) any {
		return put(layout.NewSpacer())
	})

	fnNewHBoxLayout := pre("new-hbox-layout")
	interp.Def(fnNewHBoxLayout, 0, func(a []any) any {
		l := layout.NewHBoxLayout()
		return put(&l)
	})

	fnNewVBoxLayout := pre("new-vbox-layout")
	interp.Def(fnNewVBoxLayout, 0, func(a []any) any {
		l := layout.NewVBoxLayout()
		return put(&l)
	})

	fnNewHScroll := pre("new-hscroll")
	interp.Def(fnNewHScroll, 1, func(a []any) any {
		obj := mustGet1(fnNewHScroll, "GUI canvas object ID", a[0]).(fyne.CanvasObject)
		return put(container.NewHScroll(obj))
	})

	fnNewScroll := pre("new-scroll")
	interp.Def(fnNewScroll, 1, func(a []any) any {
		obj := mustGet1(fnNewScroll, "GUI canvas object ID", a[0]).(fyne.CanvasObject)
		return put(container.NewScroll(obj))
	})

	fnNewVScroll := pre("new-vscroll")
	interp.Def(fnNewVScroll, 1, func(a []any) any {
		obj := mustGet1(fnNewVScroll, "GUI canvas object ID", a[0]).(fyne.CanvasObject)
		return put(container.NewVScroll(obj))
	})

	fnGetScrollOffset := pre("get-scroll-offset")
	interp.Def(fnGetScrollOffset, 1, func(a []any) any {
		obj := mustGet1(fnGetScrollOffset, "GUI scroll ID", a[0]).(*container.Scroll)
		offset := obj.Offset
		return &z3.Cell{Car: goarith.AsNumber(float64(offset.X)),
			Cdr: &z3.Cell{Car: goarith.AsNumber(float64(offset.Y)), Cdr: z3.Nil}}
	})

	fnSetScrollOffset := pre("set-scroll-offset")
	interp.Def(fnSetScrollOffset, 2, func(a []any) any {
		obj := mustGet1(fnSetScrollOffset, "GUI scroll ID", a[0]).(*container.Scroll)
		offset, ok := MustGetPosition(fnSetScrollOffset, 1, a[1])
		if !ok {
			panic(fmt.Sprintf(fnSetScrollOffset+": expected valid position as second argument, given %v",
				z3.Str(a[1])))
		}
		obj.Offset = offset
		return z3.Void
	})

	fnNewGridLayout := pre("new-grid-layout")
	interp.Def(fnNewGridLayout, 1, func(a []any) any {
		n := z3.ToInt64(fnNewGridLayout, a[0])
		return put(layout.NewGridLayout(int(n)))
	})

	fnNewGridWrapLayout := pre("new-grid-wrap-layout")
	interp.Def(fnNewGridWrapLayout, 2, func(a []any) any {
		w := z3.ToFloat64(a[0])
		h := z3.ToFloat64(a[1])
		return put(layout.NewGridWrapLayout(fyne.NewSize(float32(w), float32(h))))
	})

	fnNewFormLayout := pre("new-form-layout")
	interp.Def(fnNewFormLayout, 0, func(a []any) any {
		return put(layout.NewFormLayout())
	})

	fnNewCenterLayout := pre("new-center-layout")
	interp.Def(fnNewCenterLayout, 0, func(a []any) any {
		return put(layout.NewCenterLayout())
	})

	fnNewStackLayout := pre("new-stack-layout")
	interp.Def(fnNewStackLayout, 0, func(a []any) any {
		return put(layout.NewStackLayout())
	})

	// CONTAINER

	fnNewContainer := pre("new-container")
	interp.Def(fnNewContainer, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		layout := mustGet1(fnNewContainer, "layout", li.Car).(*fyne.Layout)
		li = li.CdrCell()
		objs := make([]fyne.CanvasObject, 0, len(a)-1)
		for li != z3.Nil {
			obj, ok := get(li.Car)
			if !ok {
				panic(fmt.Sprintf(fnNewContainer+": unknown GUI object ID, given %v", z3.Str(li.Car)))
			}
			objs = append(objs, obj.(fyne.CanvasObject))
			li = li.CdrCell()
		}
		c := container.New(*layout, objs...)
		return put(c)
	})

	fnNewContainerWithoutLayout := pre("new-container-without-layout")
	interp.Def(fnNewContainerWithoutLayout, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		objs := make([]fyne.CanvasObject, 0, len(a)-1)
		for li != z3.Nil {
			obj, ok := get(li.Car)
			if !ok {
				panic(fmt.Sprintf(fnNewContainerWithoutLayout+": unknown GUI object ID, given %v", z3.Str(li.Car)))
			}
			objs = append(objs, obj.(fyne.CanvasObject))
			li = li.CdrCell()
		}
		c := container.NewWithoutLayout(objs...)
		return put(c)
	})

	fnNewBorder := pre("new-border")
	// (new-border top bottom left right obj...) => int
	interp.Def(fnNewBorder, -1, func(a []any) any {
		arr := make([]fyne.CanvasObject, 0)
		args := z3.ListToArray(a[0].(*z3.Cell))
		if len(args) < 4 {
			panic(fmt.Sprintf(fnNewBorder+": requires at least top, bottom, left, right arguments, given: %v",
				a[0].(*z3.Cell)))
		}
		for _, arg := range args {
			if cell, ok := arg.(*z3.Cell); ok {
				if cell != z3.Nil {
					panic(fmt.Sprintf(fnNewBorder+": expected a valid GUI object ID or nil, given a non-nil list: %v",
						z3.Str(arg)))
				}
				arr = append(arr, nil)
				continue
			}
			obj, ok := get(arg)
			if !ok {
				panic(fmt.Sprintf(fnNewBorder+": expected a valid GUI object ID or nil, given: %v",
					z3.Str(arg)))
			}
			if canvas, ok := obj.(fyne.CanvasObject); ok {
				arr = append(arr, canvas)
				continue
			}
			panic(fmt.Sprintf(fnNewBorder+": expected a valid GUI canvas object, but the given %v is not a canvas object", z3.Str(arg)))
		}
		return put(container.NewBorder(arr[0], arr[1], arr[2], arr[3], arr[4:]...))
	})

	fnNewTabitem := pre("new-tabitem")
	interp.Def(fnNewTabitem, 2, func(a []any) any {
		title := a[0].(string)
		arg := mustGet1(fnNewTabitem, "GUI canvas object ID", a[1])
		obj, ok := arg.(fyne.CanvasObject)
		if !ok {
			panic(fmt.Sprintf(fnNewTabitem+": argument must be a canvas object, given %v", z3.Str(arg)))
		}
		return put(container.NewTabItem(title, obj))
	})

	fnNewTabitemWithIcon := pre("new-tabitem-with-icon")
	interp.Def(fnNewTabitemWithIcon, 3, func(a []any) any {
		title := a[0].(string)
		icon := mustGet(fnNewTabitemWithIcon, "GUI icon resource ID", a, 1)
		ics, ok := icon.(fyne.Resource)
		if !ok {
			panic(fmt.Sprintf(fnNewTabitemWithIcon+": expected an icon resource as second argument, received: %v", z3.Str(a[1])))
		}
		canvas := mustGet(fnNewTabitemWithIcon, "GUI canvas object ID", a, 2)
		return put(container.NewTabItemWithIcon(title, ics, canvas.(fyne.CanvasObject)))
	})

	fnNewAppTabs := pre("new-app-tabs")
	// (new-app-tabs tab-item ...) => int
	interp.Def(fnNewAppTabs, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		arr := make([]*container.TabItem, 0)
		for li != z3.Nil {
			tab := mustGet1(fnNewAppTabs, "GUI tabitem ID", li.Car)
			arr = append(arr, tab.(*container.TabItem))
			li = li.CdrCell()
		}
		return put(container.NewAppTabs(arr...))
	})

	fnNewDocTabs := pre("new-doc-tabs")
	// (new-doc-tabs tab-item ...) => int
	interp.Def(fnNewDocTabs, -1, func(a []any) any {
		li := a[0].(*z3.Cell)
		arr := make([]*container.TabItem, 0)
		for li != z3.Nil {
			tab := mustGet1(fnNewDocTabs, "GUI tabitem ID", li.Car)
			arr = append(arr, tab.(*container.TabItem))
			li = li.CdrCell()
		}
		return put(container.NewDocTabs(arr...))
	})

	fnNewHSplit := pre("new-hsplit")
	// (new-hsplit leading trailing) => int
	interp.Def(fnNewHSplit, 2, func(a []any) any {
		lead := mustGet(fnNewHSplit, "GUI canvas object ID", a, 0).(fyne.CanvasObject)
		trail := mustGet(fnNewHSplit, "GUI canvas object ID", a, 1).(fyne.CanvasObject)
		return put(container.NewHSplit(lead, trail))
	})

	fnNewVSplit := pre("new-vsplit")
	// (new-vsplit leading trailing) => int
	interp.Def(fnNewVSplit, 2, func(a []any) any {
		top := mustGet(fnNewVSplit, "GUI canvas object ID", a, 0).(fyne.CanvasObject)
		bottom := mustGet(fnNewVSplit, "GUI canvas object ID", a, 1).(fyne.CanvasObject)
		return put(container.NewVSplit(top, bottom))
	})

	fnSetSplitOffset := pre("set-split-offset")
	// (set-split-offset split fl)
	interp.Def(fnSetSplitOffset, 2, func(a []any) any {
		split := mustGet(fnSetSplitOffset, "GUI split ID", a, 0).(*container.Split)
		fl := z3.ToFloat64(a[1])
		split.SetOffset(fl)
		return z3.Void
	})

	// THEME

	fnThemeColor := pre("theme-color")
	// (theme-color selector) => li
	interp.Def(fnThemeColor, 1, func(a []any) any {
		sym := a[0].(*z3.Sym)
		var c color.Color
		switch sym {
		case ForegroundColor:
			c = theme.ForegroundColor()
		case BackgroundColor:
			c = theme.BackgroundColor()
		case ButtonColor:
			c = theme.ButtonColor()
		case DisabledButtonColor:
			c = theme.DisabledColor()
		case DisabledColor:
			c = theme.DisabledColor()
		case DisabledTextColor:
			c = theme.DisabledColor()
		case ErrorColor:
			c = theme.ErrorColor()
		case FocusColor:
			c = theme.FocusColor()
		case HoverColor:
			c = theme.HoverColor()
		case InputBackgroundColor:
			c = theme.InputBackgroundColor()
		case InputBorderColor:
			c = theme.InputBorderColor()
		case MenuBackgroundColor:
			c = theme.MenuBackgroundColor()
		case OverlayBackgroundColor:
			c = theme.OverlayBackgroundColor()
		case PlaceHolderColor:
			c = theme.PlaceHolderColor()
		case PressedColor:
			c = theme.PressedColor()
		case PrimaryColor:
			c = theme.PrimaryColor()
		case ScrollBarColor:
			c = theme.ScrollBarColor()
		case SelectionColor:
			c = theme.SelectionColor()
		case SeparatorColor:
			c = theme.SeparatorColor()
		case ShadowColor:
			c = theme.ShadowColor()
		case SuccessColor:
			c = theme.SuccessColor()
		case WarningColor:
			c = theme.WarningColor()
		case TextGridForegroundColor:
			c = widget.TextGridStyleDefault.TextColor()
		case TextGridBackgroundColor:
			c = widget.TextGridStyleDefault.BackgroundColor()
		default:
			panic(fmt.Sprintf(fnThemeColor+": theme color selector must be one of '(foreground background button disabled-button disabled disabled-text error focus hover input-background input-border menu-background overlay-background place-holder pressed primary scroll-bar selection separator shadow success warning text-grid-foreground text-grid-background), given %v", z3.Str(sym)))
		}
		return ColorToList(c)
	})

	fnThemeIsDark := pre("theme-is-dark?")
	// (theme-is-dark?) => bool
	interp.Def(fnThemeIsDark, 0, func(a []any) any {
		return z3.AsLispBool(fyne.CurrentApp().Settings().ThemeVariant() == theme.VariantDark)
	})

	// RESOURCES

	fnThemeIcon := pre("theme-icon")
	// THEME ICONS
	interp.Def(fnThemeIcon, 1, func(a []any) any {
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
			panic(fmt.Sprintf(fnThemeIcon+": unknown theme icon name, given %v", z3.Str(a[0])))
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

// ListToColor converts a Z3S5 Lisp color list to a color. A nil color interface is returned
// if an empty list is given as argument.
func ListToColor(li *z3.Cell) color.Color {
	if li == z3.Nil {
		return nil
	}
	r := z3.ToUInt16(li.Car)
	li = li.CdrCell()
	g := z3.ToUInt16(li.Car)
	li = li.CdrCell()
	b := z3.ToUInt16(li.Car)
	li = li.CdrCell()
	var alpha uint16
	if li != z3.Nil {
		alpha = z3.ToUInt16(li.Car)
	} else {
		alpha = 65365
	}
	return color.NRGBA64{R: r, G: g, B: b, A: alpha}
}

// TextGridStyleToList converts a text grid style to a Z3S5 Lisp list.
func TextGridStyleToList(s widget.TextGridStyle) *z3.Cell {
	if s == nil || s == widget.TextGridStyleDefault {
		return z3.Nil
	}

	arr := make([]any, 0, 5)

	arr = append(arr, &z3.Cell{Car: ZeditStyleBold,
		Cdr: &z3.Cell{Car: z3.AsLispBool(s.Style().Bold), Cdr: z3.Nil}})
	arr = append(arr, &z3.Cell{Car: ZeditStyleItalic,
		Cdr: &z3.Cell{Car: z3.AsLispBool(s.Style().Italic), Cdr: z3.Nil}})
	arr = append(arr, &z3.Cell{Car: ZeditStyleMonospace,
		Cdr: &z3.Cell{Car: z3.AsLispBool(s.Style().Monospace), Cdr: z3.Nil}})
	arr = append(arr, &z3.Cell{Car: ZeditStyleTextColor,
		Cdr: &z3.Cell{Car: ColorToList(s.TextColor()), Cdr: z3.Nil}})
	arr = append(arr, &z3.Cell{Car: ZeditStyleBackgroundColor,
		Cdr: &z3.Cell{Car: ColorToList(s.BackgroundColor()), Cdr: z3.Nil}})
	return z3.ArrayToList(arr)
}

// ListToTextGridStyle converts a list in the format returned by TextGridStyleToList back to
// a Fyne text grid style.
func ListToTextGridStyle(caller string, arg any) widget.TextGridStyle {
	if li, ok := arg.(*z3.Cell); ok {
		if li == z3.Nil {
			return widget.TextGridStyleDefault
		}
		sty := &widget.CustomTextGridStyle{}
		for {
			if li == z3.Nil {
				break
			}
			elem := li.Car
			li2, ok := elem.(*z3.Cell)
			if !ok {
				panic(fmt.Sprintf("%v: expected an a-list with valid style information, given %v", caller, z3.Str(arg)))
			}
			head, ok := li2.Car.(*z3.Sym)
			if !ok {
				log.Printf("*WARN* %v: malformed text grid style %v in a-list %v (incompatible version?)\n", caller,
					z3.Str(li2), z3.Str(li))
				continue
			}

			switch head {
			case ZeditStyleBold:
				sty.TextStyle.Bold = z3.ToBool(li.CdrCell().Car)
			case ZeditStyleItalic:
				sty.TextStyle.Italic = z3.ToBool(li.CdrCell().Car)
			case ZeditStyleMonospace:
				sty.TextStyle.Monospace = z3.ToBool(li.CdrCell().Car)
			case ZeditStyleTextColor:
				sty.FGColor = ListToColor(li2.CdrCell().Car.(*z3.Cell))
			case ZeditStyleBackgroundColor:
				sty.BGColor = ListToColor(li2.CdrCell().Car.(*z3.Cell))
			}

			li = li.CdrCell()
		}
		return sty
	}
	return widget.TextGridStyleDefault
}

// ListToZeditStyle converts a list of colors to a zedit.EditorStyle used by zedit.Editor.
func ListToZeditStyle(caller string, arg any) zedit.Style {
	sty := zedit.Style{Monospace: true,
		FGColor: theme.TextColor(),
		BGColor: theme.InputBackgroundColor()}
	if li, ok := arg.(*z3.Cell); ok {
		for {
			if li == z3.Nil {
				break
			}
			elem := li.Car
			li2, ok := elem.(*z3.Cell)
			if !ok {
				panic(fmt.Sprintf("%v: expected an a-list with valid style information, given %v", caller, z3.Str(arg)))
			}
			if li2 == z3.Nil {
				break
			}
			head, ok := li2.Car.(*z3.Sym)
			if !ok {
				log.Printf("*WARN* %v: malformed zedit style %v in a-list %v (incompatible version?)\n", caller,
					z3.Str(li2), z3.Str(li))
				continue
			}

			switch head {
			case ZeditStyleBold:
				sty.Bold = z3.ToBool(li2.CdrCell().Car)
			case ZeditStyleItalic:
				sty.Italic = z3.ToBool(li2.CdrCell().Car)
			case ZeditStyleMonospace:
				sty.Monospace = z3.ToBool(li2.CdrCell().Car.(*z3.Cell))
			case ZeditStyleTextColor:
				sty.FGColor = ListToColor(li2.CdrCell().Car.(*z3.Cell))
			case ZeditStyleBackgroundColor:
				sty.BGColor = ListToColor(li2.CdrCell().Car.(*z3.Cell))
			}

			li = li.CdrCell()
		}
		return sty
	}
	return sty
}

// ListToTextGridCell converts a list in the format (rune style-list) to a text grid
// TextGridCell.
func ListToTextGridCell(caller string, arg any) widget.TextGridCell {
	li := arg.(*z3.Cell)
	var cell widget.TextGridCell
	cell.Rune = []rune(li.Car.(string))[0]
	li = li.CdrCell()
	cell.Style = ListToTextGridStyle(caller, li.Car)
	return cell
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

// FindTextGridParagraphStart finds the start row of the paragraph in which row is located.
// If the row is 0, 0 is returned, otherwise this checks for the next line ending with lf and
// returns the row after it.
func FindTextGridParagraphStart(grid *widget.TextGrid, row int, lf rune) int {
	if row <= 0 {
		return 0
	}
	k := len(grid.Rows[row-1].Cells)
	if k == 0 {
		return row
	}
	if grid.Rows[row-1].Cells[k-1].Rune == lf {
		return row
	}
	return FindTextGridParagraphStart(grid, row-1, lf)
}

// FindTextGridParagraphEnd finds the end row of the paragraph in which row is located.
// If row is the last row, then it is returned. Otherwise, it checks for the next row that
// ends in lf (which may be the row with which this function was called).
func FindTextGridParagraphEnd(grid *widget.TextGrid, row int, lf rune) int {
	if row >= len(grid.Rows)-1 {
		return row
	}
	k := len(grid.Rows[row].Cells)
	if k == 0 {
		return row
	}
	if grid.Rows[row].Cells[k-1].Rune == lf {
		return row
	}
	return FindTextGridParagraphEnd(grid, row+1, lf)
}

// GetTextGridParagraph returns the styled text of the paragraph as a list of cells,
// ignoring any row styles (the caller of this function needs to take care of proper
// handling of row styles themselves).
func GetTextGridParagraph(grid *widget.TextGrid, first, last int) []widget.TextGridCell {
	cells := make([]widget.TextGridCell, 0, (last-first+1)*200)
	for i := first; i <= last; i++ {
		for j := range grid.Rows[i].Cells {
			cell := grid.Rows[i].Cells[j]
			cells = append(cells, cell)
		}
	}
	return cells
}

// ad hoc struct for holding text grid cells plus hosuekeeping info
type xCell struct {
	Cell         widget.TextGridCell
	Row          *widget.TextGridRow
	IsCursorCell bool
}

// WordWrapTextGridRows word wraps a number of text grid rows, making sure soft line breaks are adjusted
// and removed accordingly. The number of rows returned may be larger then the number of rows
// provided as an argument. The position of the original cursor row and column is returned.
func WordWrapTextGridRows(rows []widget.TextGridRow, wrapCol int,
	softWrap bool, hardLF, softLF rune, cursorRow, cursorCol int) ([]widget.TextGridRow, int, int) {
	para := make([]xCell, 0)
	// 1. push all characters into one array of extended cells
	// but ignore line breaks
	cursorToNext := false
	for i := range rows {
		for j, c := range rows[i].Cells {
			isCursor := false
			if (i == cursorRow && j == cursorCol) || cursorToNext {
				isCursor = true
				cursorToNext = false
			}
			if (c.Rune == hardLF && j == len(rows[i].Cells)-1) || c.Rune == softLF {
				if i == cursorRow && j == cursorCol {
					cursorToNext = true // delete LF but make sure cursor will be on next char
				}
			} else {
				para = append(para, xCell{Cell: c,
					Row: &rows[i], IsCursorCell: isCursor})
			}
		}
	}
	// 2. now word break the paragraph and push into a result array
	// adding soft line breaks, and the final hard line break
	result := make([]widget.TextGridRow, 0)
	lastSpc := 0
	line := make([]xCell, 0, wrapCol+1)
	var overflow []xCell
	col := 0
	newCol := cursorCol
	newRow := 0
	var currentRow widget.TextGridRow
	var handled bool
	lpos := 0
	lineIdx := 0
	for i := range para {
		handled = false
		c := para[i]
		lpos++
		line = append(line, c)
		if unicode.IsSpace(c.Cell.Rune) {
			lastSpc = lpos // space position + 1 because of lpos++
		}
		if lpos >= wrapCol {
			cutPos := lpos
			if lastSpc > 0 {
				cutPos = min(lpos, lastSpc)
			}
			if cutPos >= wrapCol/2 && cutPos < len(line) {
				overflow = make([]xCell, 0, len(line)-cutPos)
				overflow = append(overflow, line[cutPos:]...)
				line = line[:cutPos]
			}
			currentRow, col = xCellsToTextGridRow(line)
			if col >= 0 {
				newCol = col
			}
			result = append(result, currentRow)
			if cellsContainCursor(line) {
				newRow = lineIdx
			}
			if overflow != nil && len(overflow) > 0 {
				line = overflow
				if cellsContainCursor(line) {
					newCol = len(line) - 1
				}
				overflow = nil
				lpos = len(line)
			} else {
				line = make([]xCell, 0, wrapCol)
				handled = true
				lpos = 0
			}
			lastSpc = 0
			lineIdx++
		}
	}
	if !handled {
		currentRow, col = xCellsToTextGridRow(line)
		if col >= 0 {
			newCol = col
		}
		result = append(result, currentRow)
		if cellsContainCursor(line) {
			newRow = lineIdx
		}
	}
	for i := range result {
		if softWrap {
			result[i].Cells = append(result[i].Cells, widget.TextGridCell{Rune: softLF, Style: nil})
		} else {
			result[i].Cells = append(result[i].Cells, widget.TextGridCell{Rune: hardLF, Style: nil})
		}
	}
	k := len(result) - 1
	n := len(result[k].Cells) - 1
	result[k].Cells[n] = widget.TextGridCell{Rune: hardLF, Style: nil}
	// The following can *only* happen if the cursor was at the very last LF,
	// which had been deleted; see Step 1 above. So we set it to the pragraph end.
	if cursorToNext {
		newRow = k
		newCol = n
	}
	return result, newRow, newCol
}

func xCellsToTextGridRow(cells []xCell) (widget.TextGridRow, int) {
	if len(cells) == 0 {
		return widget.TextGridRow{Cells: make([]widget.TextGridCell, 0), Style: nil}, -1
	}
	result := make([]widget.TextGridCell, len(cells))
	col := -1
	for i, c := range cells {
		result[i] = c.Cell
		if c.IsCursorCell {
			col = i
		}
	}
	return widget.TextGridRow{Cells: result, Style: cells[0].Row.Style}, col
}

func cellsContainCursor(cells []xCell) bool {
	for _, c := range cells {
		if c.IsCursorCell {
			return true
		}
	}
	return false
}

// TextGridRangeListToRange converts a Z3S5 List containing a range of the form
// (start-row- start-column end-row end-column) of integers to the integers.
// end-row and end-column are optional.
func TextGridRangeListToRange(caller string, li *z3.Cell) (int, int, int, int) {
	startRow := z3.ToInt(caller, li.Car)
	li = li.CdrCell()
	startCol := z3.ToInt(caller, li.Car)
	li = li.CdrCell()
	if li == z3.Nil {
		return startRow, startCol, startRow, startCol
	}
	endRow := z3.ToInt(caller, li.Car)
	li = li.CdrCell()
	endCol := z3.ToInt(caller, li.Car)
	if endRow < startRow || (startRow == endRow && endCol < startCol) {
		panic(fmt.Sprintf("%v: invalid negative range - the end is before the start, given: %v", caller, z3.Str(li)))
	}
	return startRow, startCol, endRow, endCol
}

// inSelectionRange is true if row, col is within the range startRow, startCol to endRow, endCol,
// false otherwise.
func inSelectionRange(startRow, startCol, endRow, endCol, row, col int) bool {
	return (row == startRow && col >= startCol) || (row == endRow && col <= endCol) || (row > startRow && row < endRow)
}

// CharIntervalToList converts a zedit char interval to a Z3S5 list of lists.
func CharIntervalToList(interval zedit.CharInterval) *z3.Cell {
	return &z3.Cell{Car: CharPosToList(interval.Start), Cdr: &z3.Cell{Car: CharPosToList(interval.End),
		Cdr: z3.Nil}}
}

// CharPosToList converts a zedit char position to a Z3S5 list of integers.
func CharPosToList(pos zedit.CharPos) *z3.Cell {
	return &z3.Cell{Car: goarith.AsNumber(pos.Line), Cdr: &z3.Cell{Car: goarith.AsNumber(pos.Column),
		Cdr: &z3.Cell{Car: z3.AsLispBool(pos.IsLineNumber), Cdr: z3.Nil}}}
}

// ListToCharPos converts a Z3S5 Lisp to a zedit char position, panicking if the list is malformed.
// The parsing is permissive, a list (n) returns a line number position, (n m) returns a non-line number position,
// and (n m b) returns a position where b is interpreted as bool.
func ListToCharPos(caller string, li *z3.Cell) zedit.CharPos {
	if li == z3.Nil {
		panic(fmt.Errorf("%v: not a valid zedit char position, given %v", caller, z3.Str(li)))
	}
	line := z3.ToInt(caller, li.Car)
	li = li.CdrCell()
	if li == z3.Nil {
		return zedit.CharPos{Line: line, Column: 0, IsLineNumber: true}
	}
	column := z3.ToInt(caller, li.Car)
	li = li.CdrCell()
	if li == z3.Nil {
		return zedit.CharPos{Line: line, Column: column, IsLineNumber: false}
	}
	return zedit.CharPos{Line: line, Column: column, IsLineNumber: z3.ToBool(li.Car)}
}

// ListToCharInterval converts a Z3S5 Lisp to a zedit char interval, panicking if the list is malformed.
// The parsing is permissive, if only one char position is given as list, the point interval (pos1...pos1)
// is returned. (Zedit char intervals are right-end inclusive, so this make sense.)
func ListToCharInterval(caller string, li *z3.Cell) zedit.CharInterval {
	if li == z3.Nil {
		panic(fmt.Errorf("%v: not a valid zedit char interval, given %v", caller, z3.Str(li)))
	}
	pos1 := ListToCharPos(caller, li.Car.(*z3.Cell))
	li = li.CdrCell()
	if li == z3.Nil {
		return zedit.CharInterval{Start: pos1, End: pos1}
	}
	pos2 := ListToCharPos(caller, li.Car.(*z3.Cell))
	return zedit.CharInterval{Start: pos1, End: pos2}
}

// CaretMovementSymToCaretMovement converts a Z3S5 Lisp symbol for the caret movement to
// a zedit CaretMovement int, panicking if the movement symbol is not supported.
func CaretMovementSymToCaretMovement(caller string, sym *z3.Sym) zedit.CaretMovement {
	var n zedit.CaretMovement
	switch sym {
	case CaretDown:
		n = zedit.CaretDown
	case CaretUp:
		n = zedit.CaretUp
	case CaretLeft:
		n = zedit.CaretLeft
	case CaretRight:
		n = zedit.CaretRight
	case CaretHome:
		n = zedit.CaretHome
	case CaretEnd:
		n = zedit.CaretEnd
	case CaretLineStart:
		n = zedit.CaretLineStart
	case CaretLineEnd:
		n = zedit.CaretLineEnd
	case CaretHalfPageDown:
		n = zedit.CaretHalfPageDown
	case CaretHalfPageUp:
		n = zedit.CaretHalfPageUp
	case CaretPageDown:
		n = zedit.CaretPageDown
	case CaretPageUp:
		n = zedit.CaretPageUp
	default:
		panic(fmt.Errorf("%v: zedit caret movement selector must be one of '(down up left right home end line-start line-end half-page-down half-page-up page-down page-up), given %v", caller, z3.Str(sym)))
	}
	return n
}

func TagEventToTagEventSymbol(evt zedit.TagEvent) *z3.Sym {
	switch evt {
	case zedit.CaretEnterEvent:
		return TagEvtCaretEnter
	case zedit.CaretLeaveEvent:
		return TagEvtCaretLeave
	default:
		return TagEvtUnknown
	}
}

// zedit Config properties

func SetZeditConfig(interp *z3.Interp, caller string, z *zedit.Editor, sel, prop any) {
	sym, ok := sel.(*z3.Sym)
	if !ok {
		panic(fmt.Errorf("%v: expected valid property selector, given %v", caller, z3.Str(sel)))
	}
	switch sym {
	case ZeditDrawCaret:
		z.Config.DrawCaret = z3.ToBool(prop)
	case ZeditHighlightParenRange:
		z.Config.HighlightParenRange = z3.ToBool(prop)
	case ZeditHighlightParens:
		z.Config.HighlightParens = z3.ToBool(prop)
	case ZeditLineWrap:
		z.Config.LineWrap = z3.ToBool(prop)
	case ZeditSoftWrap:
		z.Config.SoftWrap = z3.ToBool(prop)
	case ZeditShowlineNumbers:
		z.Config.ShowLineNumbers = z3.ToBool(prop)
	case ZeditShowWhitespace:
		z.Config.ShowWhitespace = z3.ToBool(prop)
	case ZeditParagraphLineNumbers:
		z.Config.ParagraphLineNumbers = z3.ToBool(prop)
	case ZeditSelectionTag:
		tag := mustGet1(caller, "GUI tag ID", prop).(zedit.Tag)
		z.Config.SelectionTag = tag
	case ZeditSelectionStyle:
		styler := mustGet1(caller, "GUI zedit style ID", prop).(zedit.TagStyler)
		z.Config.SelectionStyler = styler
	case ZeditHighlightTag:
		tag := mustGet1(caller, "GUI tag ID", prop).(zedit.Tag)
		z.Config.HighlightTag = tag
	case ZeditHighlightStyle:
		styler := mustGet1(caller, "GUI zedit style ID", prop).(zedit.TagStyler)
		z.Config.HighlightStyler = styler
	case ZeditMarkTag:
		tag := mustGet1(caller, "GUI tag ID", prop).(zedit.Tag)
		z.Config.MarkTag = tag
	case ZeditMarkTags:
		li := prop.(*z3.Cell)
		tags := make([]zedit.Tag, 0)
		for li != z3.Nil {
			tags = append(tags, li.Car.(zedit.Tag))
			li = li.CdrCell()
		}
		z.Config.MarkTags = tags
	case ZeditMarkStyle:
		styler := mustGet1(caller, "GUI zedit style ID", prop).(zedit.TagStyler)
		z.Config.MarkStyler = styler
	case ZeditErrorTag:
		tag := mustGet1(caller, "GUI tag ID", prop).(zedit.Tag)
		z.Config.ErrorTag = tag
	case ZeditErrorStyle:
		styler := mustGet1(caller, "GUI zedit style ID", prop).(zedit.TagStyler)
		z.Config.ErrorStyler = styler
	case ZeditParenErrorTag:
		tag := mustGet1(caller, "GUI tag ID", prop).(zedit.Tag)
		z.Config.ParenErrorTag = tag
	case ZeditBlendFG:
		n := z3.ToInt(caller, prop)
		z.Config.BlendFG = zedit.BlendMode(n)
	case ZeditBlendBG:
		n := z3.ToInt(caller, prop)
		z.Config.BlendBG = zedit.BlendMode(n)
	case ZeditBlendFGSwitched:
		z.Config.BlendFGSwitched = z3.ToBool(prop)
	case ZeditBlendBGSwitched:
		z.Config.BlendBGSwitched = z3.ToBool(prop)
	case ZeditGetWordAtLeft:
		z.Config.GetWordAtLeft = z3.ToBool(prop)
	case ZeditLiberalGetWordAt:
		z.Config.LiberalGetWordAt = z3.ToBool(prop)
	case ZeditSoftLF:
		z.Config.SoftLF = rune(prop.(string)[0])
	case ZeditHardLF:
		z.Config.HardLF = rune(prop.(string)[0])
	case ZeditScrollFactor:
		z.Config.ScrollFactor = float32(z3.ToFloat64(prop))
	case ZeditTabWidth:
		z.Config.TabWidth = z3.ToInt(caller, prop)
	case ZeditMinRefreshInterval:
		z.Config.MinRefreshInterval = time.Duration(z3.ToInt64(caller, prop))
	case ZeditCharDrift:
		z.Config.CharDrift = float32(z3.ToFloat64(prop))
	case ZeditCaretBlinkDelay:
		z.Config.CaretBlinkDelay = time.Duration(z3.ToInt64(caller, prop))
	case ZeditCaretOnDuration:
		z.Config.CaretOnDuration = time.Duration(z3.ToInt64(caller, prop))
	case ZeditCaretOffDuration:
		z.Config.CaretOffDuration = time.Duration(z3.ToInt64(caller, prop))
	case ZeditMaxLines:
		z.Config.MaxLines = z3.ToInt64(caller, prop)
	case ZeditMaxColumns:
		z.Config.MaxColumns = z3.ToInt64(caller, prop)
	case ZeditMaxPrintLines:
		z.Config.MaxPrintLines = z3.ToInt(caller, prop)
	case ZeditTagPreWrite:
		proc := prop.(*z3.Closure)
		z.Config.TagPreWrite = zedit.TagPreWriteFunc(func(tag zedit.TagWithInterval) error {
			lTag := getIDOrPut(tag.Tag)
			lInterval := CharIntervalToList(tag.Interval)
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: lTag, Cdr: &z3.Cell{Car: z3.QqQuote(lInterval), Cdr: z3.Nil}}}
			interp.SafeEvalWithInfo(li, z3.Nil,
				"%v\n"+fmt.Sprintf("IN zedit config '"+z3.Str(ZeditTagPreWrite)))
			return nil
		})
	case ZeditTagPostRead:
		proc := prop.(*z3.Closure)
		z.Config.TagPostRead = zedit.TagPostReadFunc(func(tag zedit.TagWithInterval) error {
			lTag := getIDOrPut(tag.Tag)
			lInterval := CharIntervalToList(tag.Interval)
			li := &z3.Cell{Car: proc, Cdr: &z3.Cell{Car: lTag, Cdr: &z3.Cell{Car: z3.QqQuote(lInterval), Cdr: z3.Nil}}}
			interp.SafeEvalWithInfo(li, z3.Nil,
				"%v\n"+fmt.Sprintf("IN zedit config '"+z3.Str(ZeditTagPostRead)))
			return nil
		})
	case ZeditCustomSave:
		proc := prop.(*z3.Closure)
		z.Config.CustomSaver = zedit.CustomSaveFunc(func(enc *json.Encoder) error {
			result, _ := interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: z3.Nil}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN zedit config '"+z3.Str(ZeditCustomSave)))
			s := fmt.Sprintf("%v", z3.Str2(result, true))
			return enc.Encode(s)
		})
	case ZeditCustomLoader:
		proc := prop.(*z3.Closure)
		z.Config.CustomLoader = zedit.CustomLoadFunc(func(dec *json.Decoder) error {
			var s string
			err := dec.Decode(&s)
			if err != nil {
				return err
			}
			reader := z3.NewReader(strings.NewReader(s),
				z3.NewInternalSource("str->expr/"+ZeditCustomLoader.String(), s))
			x, ex := reader.Read()
			if ex != nil {
				return ex.(error)
			}
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: x, Cdr: z3.Nil}}, z3.Nil,
				"%v\n"+fmt.Sprintf("IN zedit config '%v", z3.Str(ZeditCustomLoader)))
			return nil
		})
	default:
		panic(fmt.Errorf("%v: expected valid property selector, given %v", caller, z3.Str(sel)))
	}
}

func GetZeditConfig(interp *z3.Interp, caller string, z *zedit.Editor, sel any) any {
	sym, ok := sel.(*z3.Sym)
	if !ok {
		panic(fmt.Errorf("%v: expected valid property selector, given %v", caller, z3.Str(sel)))
	}
	switch sym {
	case ZeditDrawCaret:
		return z3.AsLispBool(z.Config.DrawCaret)
	case ZeditHighlightParenRange:
		return z3.AsLispBool(z.Config.HighlightParenRange)
	case ZeditHighlightParens:
		return z3.AsLispBool(z.Config.HighlightParens)
	case ZeditLineWrap:
		return z3.AsLispBool(z.Config.LineWrap)
	case ZeditSoftWrap:
		return z3.AsLispBool(z.Config.SoftWrap)
	case ZeditShowlineNumbers:
		return z3.AsLispBool(z.Config.ShowLineNumbers)
	case ZeditShowWhitespace:
		return z3.AsLispBool(z.Config.ShowWhitespace)
	case ZeditParagraphLineNumbers:
		return z3.AsLispBool(z.Config.ParagraphLineNumbers)
	case ZeditGetWordAtLeft:
		return z3.AsLispBool(z.Config.GetWordAtLeft)
	case ZeditLiberalGetWordAt:
		return z3.AsLispBool(z.Config.LiberalGetWordAt)
	case ZeditSelectionTag:
		return getIDOrPut(z.Config.SelectionTag)
	case ZeditSelectionStyle:
		return getIDOrPut(z.Config.SelectionStyler)
	case ZeditHighlightTag:
		return getIDOrPut(z.Config.HighlightTag)
	case ZeditSelectionStyle:
		return getIDOrPut(z.Config.HighlightStyler)
	case ZeditMarkTag:
		return getIDOrPut(z.Config.MarkTag)
	case ZeditMarkTags:
		arr := make([]any, len(z.Config.MarkTags))
		for i := range z.Config.MarkTags {
			id := getIDOrPut(z.Config.MarkTags[i])
			arr[i] = id
		}
		return z3.ArrayToList(arr)
	case ZeditMarkStyle:
		return getIDOrPut(z.Config.MarkStyler)
	case ZeditErrorTag:
		return getIDOrPut(z.Config.ErrorTag)
	case ZeditErrorStyle:
		return getIDOrPut(z.Config.ErrorStyler)
	case ZeditParenErrorTag:
		return getIDOrPut(z.Config.ErrorTag)
	case ZeditBlendFG:
		return goarith.AsNumber(z.Config.BlendFG)
	case ZeditBlendBG:
		return goarith.AsNumber(z.Config.BlendBG)
	case ZeditBlendFGSwitched:
		return z3.AsLispBool(z.Config.BlendFGSwitched)
	case ZeditBlendBGSwitched:
		return z3.AsLispBool(z.Config.BlendBGSwitched)
	case ZeditSoftLF:
		return strconv.QuoteRune(z.Config.SoftLF)
	case ZeditHardLF:
		return strconv.QuoteRune(z.Config.HardLF)
	case ZeditScrollFactor:
		return goarith.AsNumber(float64(z.Config.ScrollFactor))
	case ZeditTabWidth:
		return goarith.AsNumber(z.Config.TabWidth)
	case ZeditMinRefreshInterval:
		return goarith.AsNumber(int64(z.Config.MinRefreshInterval))
	case ZeditCharDrift:
		return goarith.AsNumber(float64(z.Config.CharDrift))
	case ZeditCaretBlinkDelay:
		return goarith.AsNumber(int64(z.Config.CaretBlinkDelay))
	case ZeditCaretOnDuration:
		return goarith.AsNumber(int64(z.Config.CaretOnDuration))
	case ZeditCaretOffDuration:
		return goarith.AsNumber(int64(z.Config.CaretOffDuration))
	case ZeditMaxLines:
		return goarith.AsNumber(int64(z.Config.MaxLines))
	case ZeditMaxColumns:
		return goarith.AsNumber(int64(z.Config.MaxColumns))
	case ZeditMaxPrintLines:
		return goarith.AsNumber(int64(z.Config.MaxPrintLines))
	case ZeditTagPreWrite:
		if v, ok := interp.GetGlobalVar(ZeditTagPreWrite); ok {
			return v
		}
		interp.Def(ZeditTagPreWriteIntrinsic.String(), 2, func(a []any) any {
			tag := mustGet(ZeditTagPreWriteIntrinsic.String(), "GUI tag ID", a, 0).(zedit.Tag)
			interval := ListToCharInterval(ZeditTagPreWriteIntrinsic.String(), a[1].(*z3.Cell))
			result := z.Config.TagPreWrite(zedit.TagWithInterval{Tag: tag, Interval: interval})
			if result != nil {
				panic(fmt.Sprintf(ZeditTagPreWriteIntrinsic.String()+": %v", z3.Str(result)))
			}
			return z3.Void
		})
		result, _ := interp.SafeEvalWithInfo(ZeditTagPreWriteIntrinsic, z3.Nil,
			"%v\n"+fmt.Sprintf("IN zedit config '"+z3.Str(ZeditTagPreWrite)))
		return result
	case ZeditTagPostRead:
		if v, ok := interp.GetGlobalVar(ZeditTagPostRead); ok {
			return v
		}
		interp.Def(ZeditTagPostReadIntrinsic.String(), 2, func(a []any) any {
			tag := mustGet(ZeditTagPostReadIntrinsic.String(), "GUI tag ID", a, 0).(zedit.Tag)
			interval := ListToCharInterval(ZeditTagPostReadIntrinsic.String(), a[1].(*z3.Cell))
			result := z.Config.TagPostRead(zedit.TagWithInterval{Tag: tag, Interval: interval})
			if result != nil {
				panic(fmt.Sprintf(ZeditTagPostReadIntrinsic.String()+": %v", z3.Str(result)))
			}
			return z3.Void
		})
		result, _ := interp.SafeEvalWithInfo(ZeditTagPostReadIntrinsic, z3.Nil,
			"%v\n"+fmt.Sprintf("IN zedit config '"+z3.Str(ZeditTagPostRead)))
		return result
	case ZeditCustomSave:
		if v, ok := interp.GetGlobalVar(ZeditCustomSaveIntrinsic); ok {
			return v
		}
		return z3.Nil
	case ZeditCustomLoader:
		if v, ok := interp.GetGlobalVar(ZeditCustomLoadIntrinsic); ok {
			return v
		}
		return z3.Nil
	default:
		panic(fmt.Errorf("%v: expected valid property selector, given %v", caller, z3.Str(sel)))
	}
}

func SymToEditorEvent(caller string, s any) zedit.EditorEvent {
	sym := s.(*z3.Sym)
	switch sym {
	case EditorCaretMove:
		return zedit.CaretMoveEvent
	case EditorWordChange:
		return zedit.WordChangeEvent
	case EditorSelectWord:
		return zedit.SelectWordEvent
	default:
		panic(fmt.Sprintf("%v: unknown editor event '%v", caller, z3.Str(sym)))
	}
}

func EditorEventToSym(caller string, evt zedit.EditorEvent) *z3.Sym {
	switch evt {
	case zedit.CaretMoveEvent:
		return EditorCaretMove
	case zedit.WordChangeEvent:
		return EditorWordChange
	case zedit.SelectWordEvent:
		return EditorSelectWord
	default:
		return EditorUnknown
	}
}
