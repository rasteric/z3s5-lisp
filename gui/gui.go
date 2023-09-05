package ui

import (
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
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/widget"
	"github.com/nukata/goarith"
	z3 "github.com/rasteric/z3s5-lisp"
)

var counter uint64
var storage sync.Map
var revstore sync.Map
var apl fyne.App
var mainWin fyne.Window

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

// DefUI defines the user interface functions. If you want to avoid polluting the namespace, use
// a config with a custom Prefix. Use DefaultConfig for a maximally permissive default configuration.
// Various security-sensitive settings such as allowing or disallowing creation of new windows can be adjusted
// in the Config. If you set these, be sure to also restrict the language using Z3S5 Lisp standard security tools,
// such as unbinding certain functions and then protecting them and disallowing unprotecting them again.
func DefUI(interp *z3.Interp, config Config) {

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

	// (set-window-size <window> <width> <height>)
	interp.Def(pre("set-window-size"), 3, func(a []any) any {
		win := mustGet(pre("set-window-size"), "GUI window ID", a, 0).(fyne.Window)
		win.Resize(fyne.NewSize(float32(z3.ToFloat64(a[1])), float32(z3.ToFloat64(a[2]))))
		return z3.Void
	})

	// (close-window <window>)
	interp.Def(pre("close-window"), 1, func(a []any) any {
		win := mustGet(pre("close-window"), "window", a, 0)
		win.(fyne.Window).Close()
		clear(win)
		return z3.Void
	})

	// (show-window <window>)
	interp.Def(pre("show-window"), 1, func(a []any) any {
		win := mustGet(pre("show-window"), "window", a, 0)
		win.(fyne.Window).Show()
		return z3.Void
	})

	// (resize-window <window> <width-float> <height-float>)
	interp.Def(pre("resize-window"), 3, func(a []any) any {
		win := mustGet(pre("resize-window"), "window", a, 0)
		w := z3.ToFloat64(a[1])
		h := z3.ToFloat64(a[2])
		win.(fyne.Window).Resize(fyne.NewSize(float32(w), float32(h)))
		return z3.Void
	})

	// (hide-window <window>)
	interp.Def(pre("hide-window"), 1, func(a []any) any {
		win := mustGet(pre("hide-window"), "window", a, 0)
		win.(fyne.Window).Hide()
		return z3.Void
	})

	// (set-window-on-close-callback <window> <callback>)
	interp.Def(pre("set-window-on-close-callback"), 2, func(a []any) any {
		win := mustGet(pre("set-window-on-close-callback"), "window", a, 0)
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
		e := mustGet(pre("set-entry-on-change-callback"), "entry", a, 0)
		proc := a[1].(*z3.Closure)
		e.(*widget.Entry).OnChanged = func(s string) {
			li2 := &z3.Cell{Car: s, Cdr: z3.Nil}
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		}
		return z3.Void
	})

	// TEXTGRID

	// (new-text-grid [<string>] [show-line-numbers|show-whitespace|tab-width <int>])
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
				interp.Eval(&z3.Cell{Car: uproc, Cdr: &z3.Cell{Car: goarith.AsNumber(id), Cdr: &z3.Cell{Car: goarith.AsNumber(n), Cdr: z3.Nil}}}, z3.Nil)
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
		// TODO requires Fyne 2.4
		// rect.CornerRadius = float32(z3.ToFloat64(li.Car))
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

	interp.Def(pre("close-ui"), 0, func(a []any) any {
		CloseUI()
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

	interp.Def(pre("new-max-layout"), 0, func(a []any) any {
		return put(layout.NewMaxLayout())
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

	// container.AppTabs
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

	// container.DocTabs
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
