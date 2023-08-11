package gui

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
	z3 "github.com/rasteric/z3s5-lisp"
)

var BoxedApp = z3.NewSym("gui.app")
var BoxedWindow = z3.NewSym("gui.window")
var BoxedLabel = z3.NewSym("gui.label")
var BoxedSize = z3.NewSym("gui.size")
var BoxedButton = z3.NewSym("gui.button")

func DefGUI(interp *z3.Interp) {

	interp.DefBoxed(BoxedApp)
	interp.DefBoxed(BoxedWindow)
	interp.DefBoxed(BoxedLabel)
	interp.DefBoxed(BoxedSize)

	// register this module
	reflect, ok := interp.GetGlobalVar(z3.ReflectSym)
	if !ok {
		reflect = z3.Nil
	}
	interp.SetGlobalVar(z3.ReflectSym, &z3.Cell{Car: z3.NewSym("gui"), Cdr: reflect})

	// the new commands

	// fyne.App
	interp.Def("gui.new-app", 1, func(a []any) any {
		apl := app.NewWithID(a[0].(string))
		return &z3.Boxed{Datum: apl, Sort: BoxedApp, Valid: true}
	})

	interp.Def("gui.app-run", 1, func(a []any) any {
		apl := z3.MustGetBoxed("gui.appp-run", a[0], BoxedApp)
		apl.Datum.(fyne.App).Run()
		return z3.Void
	})

	// fyne.Window
	interp.Def("gui.new-window", 2, func(a []any) any {
		apl := z3.MustGetBoxed("gui.new-window", a[0], BoxedApp)
		title := a[1].(string)
		win := apl.Datum.(fyne.App).NewWindow(title)
		return &z3.Boxed{Datum: win, Sort: BoxedWindow, Valid: true}
	})

	interp.Def("gui.window-set-content", 2, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-set-content", a[0], BoxedWindow)
		content := a[1].(*z3.Boxed)
		win.Datum.(fyne.Window).SetContent(content.Datum.(fyne.CanvasObject))
		return z3.Void
	})

	interp.Def("gui.window-show-and-run", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-show-and-run", a[0], BoxedWindow)
		win.Datum.(fyne.Window).ShowAndRun()
		return z3.Void
	})

	interp.Def("gui.window-close", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-close", a[0], BoxedWindow)
		win.Datum.(fyne.Window).Close()
		return z3.Void
	})

	interp.Def("gui.window-show", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-show", a[0], BoxedWindow)
		win.Datum.(fyne.Window).Show()
		return z3.Void
	})

	interp.Def("gui.window-set-master", 1, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-set-master", a[0], BoxedWindow)
		win.Datum.(fyne.Window).SetMaster()
		return z3.Void
	})

	interp.Def("gui.window-resize", 2, func(a []any) any {
		win := z3.MustGetBoxed("gui.window-resize", a[0], BoxedWindow)
		size := z3.MustGetBoxed("gui.window-resize", a[1], BoxedSize)
		win.Datum.(fyne.Window).Resize(size.Datum.(fyne.Size))
		return z3.Void
	})

	// fyne.Size
	interp.Def("gui.new-size", 2, func(a []any) any {
		return &z3.Boxed{Datum: fyne.NewSize(float32(z3.ToFloat64(a[0])), float32(z3.ToFloat64(a[1]))),
			Sort: BoxedSize, Valid: true}
	})

	// WIDGETS

	// widget.Label
	interp.Def("gui.new-label", 1, func(a []any) any {
		l := widget.NewLabel(a[0].(string))
		return &z3.Boxed{Datum: l, Sort: BoxedLabel, Valid: true}
	})

	// widget.Button
	interp.Def("gui.new-button", 2, func(a []any) any {
		box := &z3.Boxed{Datum: nil, Sort: BoxedButton, Valid: false}
		proc := a[1].(*z3.Closure)
		b := widget.NewButton(a[0].(string), func() {
			li2 := z3.Nil
			li := &z3.Cell{Car: proc, Cdr: li2}
			interp.Eval(li, z3.Nil)
		})
		box.Datum = b
		box.Valid = true
		return box
	})
}
