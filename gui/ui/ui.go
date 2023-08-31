package ui

import (
	"fmt"
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

func DefUI(interp *z3.Interp) {

	interp.Def("new-window", 1, func(a []any) any {
		return put(apl.NewWindow(a[0].(string)))
	})

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

	interp.Def("show-window", 1, func(a []any) any {
		win, ok := get(a[0])
		if !ok {
			panic(fmt.Sprintf("show-window: expected a window as argument, given %v", a[0]))
		}
		win.(fyne.Window).Show()
		return z3.Void
	})
}
