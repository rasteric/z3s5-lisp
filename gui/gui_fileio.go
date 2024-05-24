//go:build fileio

package ui

import (
	"fmt"
	"io"

	"fyne.io/fyne/v2"
	"fyne.io/fyne/v2/dialog"
	fyneStorage "fyne.io/fyne/v2/storage"
	z3 "github.com/rasteric/z3s5-lisp"
)

var GUIFileIOSym = z3.NewSym("gui-fileio")

// DefGUI defines the user interface functions. If you want to avoid polluting the namespace, use
// a config with a custom Prefix. Use DefaultConfig for a maximally permissive default configuration.
// Various security-sensitive settings such as allowing or disallowing creation of new windows can be adjusted
// in the Config. If you set these, be sure to also restrict the language using Z3S5 Lisp standard security tools,
// such as unbinding certain functions and then protecting them and disallowing unprotecting them again.
func DefGUI(interp *z3.Interp, config Config) {
	defGUINoFileIO(interp, config)
	defGUIFileIO(interp, config)
	if err := DefGUIAdditions(interp); err != nil {
		panic(fmt.Sprintf("Z3S5 Lisp internal error, failed to boot embedded GUI additions: %v", err))
	}
}

func defGUIFileIO(interp *z3.Interp, config Config) {

	// register this module
	reflect, ok := interp.GetGlobalVar(z3.ReflectSym)
	if !ok {
		reflect = z3.Nil
	}
	interp.SetGlobalVar(z3.ReflectSym, &z3.Cell{Car: GUIFileIOSym, Cdr: reflect})

	pre := func(s string) string {
		if cfg.Prefix == "" {
			return s
		}
		return cfg.Prefix + "." + s
	}

	fnShowFileOpen := pre("show-file-open")
	// (show-file-open proc win)
	interp.Def(fnShowFileOpen, 2, func(a []any) any {
		proc := a[0].(*z3.Closure)
		win := mustGet1(fnShowFileOpen, "GUI window ID", a[1]).(fyne.Window)
		if !interp.Runtime().(z3.Runtime).Perm().AllowFileRead {
			panic(fnShowFileOpen + ": security violation, you are not allowed to read files!")
		}
		dialog.ShowFileOpen(func(uri fyne.URIReadCloser, err error) {
			var lspErr any
			if err == nil {
				lspErr = z3.Nil
			} else {
				lspErr = err.Error()
			}
			var lspPort any
			if uri != nil {
				port := &z3.Boxed{Datum: uri, Sort: z3.BoxedURIPort, Valid: true}
				ioReader, _ := port.Datum.(io.Reader)
				iowriter, _ := port.Datum.(io.Writer)
				ioseeker, _ := port.Datum.(io.Seeker)
				iowriterat, _ := port.Datum.(io.WriterAt)
				stream := z3.NewStream(z3.NewFileSource(uri.URI().String()), ioReader, iowriter, ioseeker, iowriterat)
				interp.Streams().Store(port, stream)
				lspPort = port
			} else {
				lspPort = z3.Nil
			}
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: lspPort,
				Cdr: &z3.Cell{Car: lspErr, Cdr: z3.Nil}}},
				z3.Nil, "%v\n IN "+fnShowFileOpen+" callback")
		}, win)
		return z3.Void
	})

	fnShowFolderOpen := pre("show-folder-open")
	// (show-folder-open proc win)
	interp.Def(fnShowFolderOpen, 2, func(a []any) any {
		proc := a[0].(*z3.Closure)
		win := mustGet1(fnShowFolderOpen, "GUI window ID", a[1]).(fyne.Window)
		if !interp.Runtime().(z3.Runtime).Perm().AllowFileRead {
			panic(fnShowFolderOpen + ": security violation, you are not allowed to read files and that includes folders!")
		}
		dialog.ShowFolderOpen(func(uri fyne.ListableURI, err error) {
			var lspErr, id any
			if err == nil {
				lspErr = z3.Nil
			} else {
				lspErr = err.Error()
			}
			if uri != nil {
				id = uri.String()
			} else {
				id = z3.Nil
			}
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: id,
				Cdr: &z3.Cell{Car: lspErr, Cdr: z3.Nil}}},
				z3.Nil, "%v\n IN "+fnShowFileOpen+" callback")
		}, win)
		return z3.Void
	})

	fnListURI := pre("list-uri")
	// (list-uri uri) => li
	interp.Def(fnListURI, 1, func(a []any) any {
		u, err := fyneStorage.ParseURI(a[0].(string))
		if err != nil {
			panic(fnListURI + ": " + err.Error())
		}
		uri, err := fyneStorage.ListerForURI(u)
		if err != nil {
			panic(fnListURI + ": " + err.Error())
		}
		if !interp.Runtime().(z3.Runtime).Perm().AllowFileRead {
			panic(fnListURI + ": security violation, you are not allowed to read files, including the contents of folders!")
		}
		val := []any{}
		switch uri.(type) {
		case fyne.ListableURI:
			u := uri.(fyne.ListableURI)
			li, err := u.List()
			if err != nil {
				panic(fmt.Sprintf(fnListURI+": %v", err))
			}
			result := make([]any, 0, len(li))
			for i := range li {
				id := li[i].String()
				result = append(result, id)
			}
			val = result
		}
		return val
	})

	fnIsURI := pre("uri?")
	// (uri? s) => bool
	interp.Def(fnIsURI, 1, func(a []any) any {
		_, err := fyneStorage.ParseURI(a[0].(string))
		if err != nil {
			return z3.Nil
		}
		return z3.AsLispBool(true)
	})

	fnIsListableURI := pre("listable-uri?")
	// (listable-uri? s) => bool
	interp.Def(fnIsListableURI, 1, func(a []any) any {
		uri, err := fyneStorage.ParseURI(a[0].(string))
		if err != nil {
			return z3.Nil
		}
		_, err = fyneStorage.ListerForURI(uri)
		return z3.AsLispBool(err != nil)
	})

	fnShowFileSave := pre("show-file-save")
	// (show-file-save proc win)
	interp.Def(fnShowFileSave, 2, func(a []any) any {
		proc := a[0].(*z3.Closure)
		win := mustGet1(fnShowFileOpen, "GUI window ID", a[1]).(fyne.Window)
		if !interp.Runtime().(z3.Runtime).Perm().AllowFileWrite {
			panic(fnShowFileSave + ": security violation, you are not allowed to write to files!")
		}
		dialog.ShowFileSave(func(uri fyne.URIWriteCloser, err error) {
			var lspErr any
			if err == nil {
				lspErr = z3.Nil
			} else {
				lspErr = err.Error()
			}
			var lspPort any
			if uri != nil {
				port := &z3.Boxed{Datum: uri, Sort: z3.BoxedURIPort, Valid: true}
				ioReader, _ := port.Datum.(io.Reader)
				iowriter, _ := port.Datum.(io.Writer)
				ioseeker, _ := port.Datum.(io.Seeker)
				iowriterat, _ := port.Datum.(io.WriterAt)
				stream := z3.NewStream(z3.NewFileSource(uri.URI().String()), ioReader, iowriter, ioseeker, iowriterat)
				interp.Streams().Store(port, stream)
				lspPort = port
			} else {
				lspPort = z3.Nil
			}
			interp.SafeEvalWithInfo(&z3.Cell{Car: proc, Cdr: &z3.Cell{Car: lspPort,
				Cdr: &z3.Cell{Car: lspErr, Cdr: z3.Nil}}},
				z3.Nil, "%v\n IN "+fnShowFileSave+" callback")
		}, win)
		return z3.Void
	})

}
