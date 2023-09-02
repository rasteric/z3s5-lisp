package ui

import (
	"fmt"
	"net/url"
	"sync"
	"sync/atomic"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
	"github.com/nukata/goarith"
	z3 "github.com/rasteric/z3s5-lisp"
)

var counter uint64
var storage sync.Map
var apl fyne.App
var mainWin fyne.Window

type HyperlinkCheckFunc = func(url *url.URL) *url.URL
type Config struct {
	HyperlinksAllowed bool
	CheckHyperlinks   HyperlinkCheckFunc
	WindowsAllowed    bool
}

var DefaultConfig = Config{
	HyperlinksAllowed: true,
	CheckHyperlinks:   HyperlinkCheckFunc(func(url *url.URL) *url.URL { return url }),
	WindowsAllowed:    true,
}

var cfg Config

// put stores an object and returns a number in Z3S5 Lisp format.
func put(obj any) any {
	n := atomic.AddUint64(&counter, 1)
	storage.Store(n, obj)
	return goarith.AsNumber(int64(n))
}

// get retrieves an object by its number and returns the object and true if found, nil and false otherwise.
func get(n any) (any, bool) {
	k := z3.ToInt64("user interface", n)
	return storage.Load(uint64(k))
}

// mustGet attempts to get the object by number stored in array a at index n, panics otherwise.
func mustGet(caller, arg string, a []any, n int) any {
	obj, ok := get(a[n])
	if !ok {
		panic(fmt.Sprintf("%v: expected valid %v ID as argument %v, given %v", caller, arg, n+1, z3.Str(a[n])))
	}
	return obj
}

// clear delete any object association for the given numeric key.
func clear(n any) {
	k, ok := n.(goarith.Int64)
	if ok {
		storage.Delete(uint64(k))
	}
}

// RunUI initializes the user interface and starts running it. The function blocks until the application
// is quit, e.g. via ShutDownUI. Since it blocks, the Lisp interpreter must be started in parallel.
func RunUI() {
	apl = app.New()
	mainWin = apl.NewWindow("")
	mainWin.SetMaster()
	apl.Run()
}

// ShutdownUI shuts down the user interface. Any attempt to use it afterwards may result in a panic. This should
// be called before closing the application, e.g. in a defer statement.
func ShutDownUI() {
	storage.Range(func(key interface{}, value interface{}) bool {
		storage.Delete(key)
		return true
	})
	mainWin.Close()
	apl.Quit()
}

// CloseUI closes all existing windows and attempts to free existing resources but does not quit the
// the internal main application, so new windows can be opened again.
func CloseUI() {
	storage.Range(func(key interface{}, value interface{}) bool {
		if win, ok := value.(fyne.Window); ok {
			win.Hide()
		}
		return true
	})
}

func DefUI(interp *z3.Interp, config Config) {

	cfg := config

	// WINDOW

	// (new-window <title-string>)
	interp.Def("new-window", 1, func(a []any) any {
		if !cfg.WindowsAllowed {
			panic("new-window: creating new windows is not permitted!")
		}
		return put(apl.NewWindow(a[0].(string)))
	})

	// (set-window-content <window> <canvas-object>)
	interp.Def("set-window-content", 2, func(a []any) any {
		win, ok := get(a[0])
		if !ok {
			panic(fmt.Sprintf("set-window-content: no window found for %v", z3.Str(a[0])))
		}
		canvas, ok := get(a[1])
		if !ok {
			panic(fmt.Sprintf("set-window-content: no canvas object found for %v", z3.Str(a[1])))
		}
		win.(fyne.Window).SetContent(canvas.(fyne.CanvasObject))
		return z3.Void
	})

	// (close-window <window>)
	interp.Def("close-window", 1, func(a []any) any {
		win := mustGet("close-window", "window", a, 0)
		win.(fyne.Window).Close()
		clear(win)
		return z3.Void
	})

	// (show-window <window>)
	interp.Def("show-window", 1, func(a []any) any {
		win := mustGet("show-window", "window", a, 0)
		win.(fyne.Window).Show()
		return z3.Void
	})

	// (resize-window <window> <width-float> <height-float>)
	interp.Def("resize-window", 3, func(a []any) any {
		win := mustGet("resize-window", "window", a, 0)
		w := z3.ToFloat64(a[1])
		h := z3.ToFloat64(a[2])
		win.(fyne.Window).Resize(fyne.NewSize(float32(w), float32(h)))
		return z3.Void
	})

	// (hide-window <window>)
	interp.Def("hide-window", 1, func(a []any) any {
		win := mustGet("hide-window", "window", a, 0)
		win.(fyne.Window).Hide()
		return z3.Void
	})

	// (set-window-on-close-callback <window> <callback>)
	interp.Def("set-window-on-close-callback", 2, func(a []any) any {
		win := mustGet("set-window-on-close-callback", "window", a, 0)
		proc := a[1].(*z3.Closure)
		win.(fyne.Window).SetOnClosed(func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		return z3.Void
	})

	// LABEL

	// (new-label <string>)
	interp.Def("new-label", 1, func(a []any) any {
		return put(widget.NewLabel(a[0].(string)))
	})

	// ENTRY

	// (new-entry [<selector>])
	interp.Def("new-entry", -1, func(a []any) any {
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
	interp.Def("set-entry-on-change-callback", 2, func(a []any) any {
		e := mustGet("set-entry-on-change-callback", "entry", a, 0)
		proc := a[1].(*z3.Closure)
		e.(*widget.Entry).OnChanged = func(s string) {
			li2 := &z3.Cell{Car: s, Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		}
		return z3.Void
	})

	// CHECK

	// (new-check <title-string> (lambda (bool) ...))
	interp.Def("new-check", 2, func(a []any) any {
		title := a[0].(string)
		proc := a[1].(*z3.Closure)
		changed := func(b bool) {
			li2 := &z3.Cell{Car: z3.AsLispBool(b), Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		}
		return put(widget.NewCheck(title, changed))
	})

	// (new-choice <selector> <string-list> (lambda (str) ...))
	interp.Def("new-choice", 3, func(a []any) any {
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
	interp.Def("new-form", 0, func(a []any) any {
		return put(widget.NewForm())
	})

	// (append-form <form> <string> <canvas-object>)
	interp.Def("append-form", 3, func(a []any) any {
		form := mustGet("append-form", "form", a, 0)
		obj := mustGet("append-form", "canvas-object", a, 2)
		form.(*widget.Form).Append(a[1].(string), obj.(fyne.CanvasObject))
		return z3.Void
	})

	// HYPERLINK

	interp.Def("new-hyperlink", 2, func(a []any) any {
		if !cfg.HyperlinksAllowed {
			panic("new-hyperlink: hyperlinks are not permitted!")
		}
		url, err := url.Parse(a[1].(string))
		if err != nil {
			panic(fmt.Errorf(`new-hyperlink: %w, given %v`, err, a[1].(string)))
		}
		if cfg.CheckHyperlinks != nil {
			url = cfg.CheckHyperlinks(url)
		}
		if url == nil {
			panic(fmt.Sprintf(`new-hyperlink:the link URL "%v" is not permitted!`, a[1].(string)))
		}
		return put(widget.NewHyperlink(a[0].(string), url))
	})

	// BUTTON

	interp.Def("new-button", 2, func(a []any) any {
		proc := a[1].(*z3.Closure)
		b := widget.NewButton(a[0].(string), func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		return put(b)
	})

	interp.Def("close-ui", 0, func(a []any) any {
		CloseUI()
		return z3.Void
	})

}
