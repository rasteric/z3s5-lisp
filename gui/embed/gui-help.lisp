;;;; GUI help definitions
;;;; This file is embedded and loaded with function `DefGUIHelp(*z3.Interp)`.

(set-help-topic-info 'gui
		     "Graphical User Interface"
		     "This section describes the GUI-related functions. These functions are only available when Z3S5 Lisp has been compiled with the embedded GUI package. See the `z3g` executable defined in `cmd/z3g/z3g.go` for an example of how to include the GUI and start it up. The key is that the interpreter must run in its own goroutine because the GUI is blocking once it has been called. GUI functions are threadsafe, or at least as threadsafe as the underlying GUI framework. Functions defined in Lisp are derived from corresponding functions of the `Fyne` framework and listed under the 'gui label in this help system. The naming conventions for translation between Go and Lisp functions are as follows:\n\n1. Camelcase is translated to lowercase with hyphens.\n2. A function `object.VerbQualifier` becomes verb-object-qualifier.\n3. Getters are written in the form `get-object-qualifier` and setters `set-object-qualifier`.\n4. As an exception of the previous rules, when the result of a function is a bool, the form is `object-predicate?`.\n\nFyne objects are represented by integer numbers. The system internally translates between these numbers and objects. Occasionally, Fyne objects are created on the fly for performance reasons. For example, sometimes color lists of the form `(r g b a)` with integers `r`, `g`,`b`, `a` are used instead of creating and storing color objects using `(nrgba r g b a)`. There are also sometimes shortcut accessors using selector symbols and other convenience wrappers for Fyne functions. When in doubt, refer to the Lisp help for details.\n\nWhen importing the GUI with `DefGUI`, a `Config` structure is provided that allows for restricted security. This makes it possible to use the GUI functions in a restricted environment that e.g. does not allow the creation of new windows.")

(defhelp new-window
    (use "(new-window title) => int")
  (info "Create a new window with #title string and return the window ID.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-content close-window show-window)))


