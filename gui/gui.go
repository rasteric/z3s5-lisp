package gui

import (
	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/app"
	"fyne.io/fyne/v2/widget"
	z3 "github.com/rasteric/z3s5-lisp"
)

var BoxedApp = z3.NewSym("__g.app")
var BoxedWindow = z3.NewSym("__g.window")
var BoxedLabel = z3.NewSym("__g.label")

func DefGUI(interp *z3.Interp) {

	interp.DefBoxed(BoxedApp)
	interp.DefBoxed(BoxedWindow)
	interp.DefBoxed(BoxedLabel)

	interp.Def("__g.new-app", 1, func(a []any) any {
		apl := app.NewWithID(a[0].(string))
		return &z3.Boxed{Datum: apl, Sort: BoxedApp, Valid: true}
	})

	interp.Def("__g.new-window", 2, func(a []any) any {
		apl := z3.MustGetBoxed("__g.new-window", a[0], BoxedApp)
		title := a[1].(string)
		win := apl.Datum.(fyne.App).NewWindow(title)
		return &z3.Boxed{Datum: win, Sort: BoxedWindow, Valid: true}
	})

	interp.Def("__g.new-label", 1, func(a []any) any {
		l := widget.NewLabel(a[0].(string))
		return &z3.Boxed{Datum: l, Sort: BoxedLabel, Valid: true}
	})

	interp.Def("__g.window-set-content", 2, func(a []any) any {
		win := z3.MustGetBoxed("__g.window-set-content", a[0], BoxedWindow)
		content := a[1].(*z3.Boxed)
		win.Datum.(fyne.Window).SetContent(content.Datum.(fyne.CanvasObject))
		return z3.Void
	})

	interp.Def("__g.window-show-and-run", 1, func(a []any) any {
		win := z3.MustGetBoxed("__g.window-show-and-run", a[0], BoxedWindow)
		win.Datum.(fyne.Window).ShowAndRun()
		return z3.Void
	})

}