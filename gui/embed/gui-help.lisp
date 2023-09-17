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

(defhelp set-window-content
    (use "(set-window-content window canvas-object)")
  (info "Set the main content of the window, which must be an existing canvas object such as a widget or container with layout.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (get-window-content get-window-canvas new-window)))

(defhelp get-window-content
    (use "(get-window-content window) => int")
  (info "Get the canvas object ID that represents the main content of the window. This is usually a widget or a container with layout.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (set-window-content get-window-canvas)))

(defhelp set-window-size
    (use "(set-window-size window width height)")
  (info "Set the size of #window to #width and #height as floats. Sizes and positions are generally given as floats whose accuracy is only guaranteed to 32 bit.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-window show-window hide-window)))

(defhelp close-window
    (use "(close-window window)")
  (info "Closes #window and delete it from internal storage. This frees window resources. It cannot be re-used after this operation. Use window-hide if you want to close/hide a window only temporarily. Notice that unlike in Fyne, there is no mechanism to close an application automatically after its main window has been closed.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (hide-window)))

(defhelp hide-window
    (use "(hide-window window)")
  (info "Hides #window. It can be shown again using show-window.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (show-window close-window)))

(defhelp set-window-on-close-callback
    (use "(set-window-on-close-callback window proc)")
  (info "Sets the close callback of #window. #proc must be a function that takes no arguments and is evaluated when the window is closed.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (show-window close-window hide-window)))

(defhelp get-window-canvas
    (use "(get-window-canvas window) => int")
  (info "Get the canvas object of #window, which is the area on which window elements are drawn. This is not the same as the window-content, which is a widget or other user interface element. The canvas is used for raw-drawing commands, for example for drawing circles and boxes. With a suitable layout that doesn't re-arrange objects, it can e.g. be used to draw overlays.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (get-window-content set-window-content)))

(defhelp get-window-title
    (use "(get-window-title window) => str")
  (info "Return the title of #window as string.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-title)))

(defhelp set-window-title
    (use "(set-window-title window title)")
  (info "Set the title of #window to string #title.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (get-window-title)))

(defhelp set-window-full-screen
    (use "(set-window-full-screen window full-screen?)")
  (info "If #full-screen? is not nil, then #window is set to full screen mode, otherwise the window is set to normal mode. In full screen mode the window is centered and fills the whole screen of the main monitor (multiple monitors are currently not supported).")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (window-full-screen? center-window-on-screen)))

(defhelp window-full-screen?
    (use "(window-full-screen? window) => bool")
  (info "Return nil if #window is full screen, true otherwise.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-full-screen center-window-on-screen)))

(defhelp set-window-fixed-size
    (use "(set-window-fixed-size window fixed-size?)")
  (info "If #fixed-size? is not nil, then #window is set to fixed size, i.e., it has no resize button and cannot be resized by the user; otherwise, the window is set to being resizable.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (window-fixed-size?)))

(defhelp window-fixed-size?
    (use "(window-fixed-size? window) => bool")
  (info "Return nil if #window is fixed size, true otherwise.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-fixed-size)))

(defhelp center-window-on-screen
    (use "(center-window-on-screen window)")
  (info "As the name implies, this function centers the window on the screen.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-full-screen window-full-screen?)))
  
(defhelp set-window-padded
    (use "(set-window-padded window padded?)")
  (info "If #padded? is not nil, then #window is set to being padded. This is the default of new window. If #padded? is nil, then the window's padding is removed, which means that the whole content area of the window can be filled with user interface elements and draw commands. This would be used for a game display, for instance.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (window-padded? set-window-full-screen window-full-screen? center-window-on-screen)))

(defhelp window-padded?
    (use "(window-padded? window) => bool")
  (info "Return nil if #window is padded, true otherwise.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-padded set-window-full-screen center-window-on-screen)))

(defhelp set-window-icon
    (use "(set-window-icon window resource)")
  (info "Set the icon of #window to the given icon #resource. Setting the icon does not guarantee that it is displayed, since this is platform-dependent.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (get-window-icon new-icon theme-icon)))

(defhelp get-window-icon
    (use "(get-window-icon window) => int")
  (info "Obtain the icon ID of the icon of #window. The resource obtained is not guaranteed to be a visible icon or might be a dummy, as not all windows have icons on all platforms.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-window-icon new-icon theme-icon)))

(defhelp set-window-main-menu
    (use "(set-window-main-menu window main-menu)")
  (info "Set the main menu of #window to #main-menu.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-main-menu new-menu new-menu*)))

(defhelp get-window-main-menu
    (use "(get-window-main-menu window) => int")
  (info "Get the main menu ID of #window.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (set-window-main-menu new-main-menu)))

(defhelp new-label
    (use "(new-label str) => int")
  (info "Creates a new text label with string #str.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-label-text)))

(defhelp set-label-text
    (use "(set-label-text label str)")
  (info "Sets the text of #label to string #str. This might resize the label depending on the layout in which the label is put.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-label)))

(defhelp new-entry
    (use "(new-entry [selector]) => int")
  (info "Create a new text entry field based on the optional #selector symbol. #selector can be a symbol in '(single-line multi-line password). The default is 'single-line.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-entry-on-change-callback set-entry-validator entry-accepts-tab? get-entry-cursor-pos set-entry-cursor-row set-entry-cursor-column
				     set-entry-on-cursor-change-callback get-entry-cursor get-entry-selected-text set-entry-min-rows-visible
				     set-entry-place-holder set-entry-text)))

(defhelp set-entry-on-change-callback
    (use "(set-entry-on-change-callback entry proc)")
  (info "Set the callback of #entry that is triggered when the entry text changes. #proc must be a procedure that takes the entry text as string.")
  (type proc)
  (arity 2)
  (topic (gui))w
  (see (new-entry set-entry-cursor-change-callback)))

(defhelp set-entry-validator
    (use "(set-entry-validator entry validator)")
  (info "Set the #validator of #entry. A validator must be created first from a special procedure or a regular expression.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-entry new-validator new-combined-string-validator new-time-validator new-regexp-validator validate-object)))

(defhelp entry-accepts-tab?
    (use "(entry-accepts-tab? entry) => bool")
  (info "Return #nil when the entry does not accept tabs, #t otherwise.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (new-entry)))

(defhelp get-entry-cursor-pos
    (use "(get-entry-cursor-pos entry) => li")
  (info "Return a list consisting of row number and column number of the current cursor position of the cursor in #entry.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (set-entry-cursor-row set-entry-cursor-column)))

(defhelp set-entry-cursor-row
    (use "(set-entry-cursor-row entry row)")
  (info "Set the row position of the cursor in #entry to integer #row.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (get-entry-cursor-pos set-entry-cursor-column)))

(defhelp set-entry-cursor-column
    (use "(set-entry-cursor-column entry column)")
  (info "Set the column position of the cursor in #entry to integer #column.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (get-entry-cursor-pos set-entry-cursor-row)))

(defhelp set-entry-on-cursor-change-callback
    (use "(set-entry-cursor-change-callback entry proc)")
  (info "Set the cursor change callback of #entry to #proc, which is a procedure that takes the entry ID as argument.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-entry set-entry-on-change-callback)))

(defhelp get-entry-cursor
    (use "(get-entry-cursor entry) => sym")
  (info "Return a symbol that represents the current cursor of #entry. Possible values are in '(default text crosshair pointer hresize vresize). Curiously, there is no way to set the cursor yet.")
  (type proc)
  (arity 1)
  (topic (gui))
  (see (new-entry)))

(defhelp set-entry-min-rows-visible
    (use "(set-entry-min-rows-visible entry rows)")
  (info "Set the minimum number of rows of #entry that are visible. This ensures that #rows text rows are visible and is a way of setting the entry's minimum size. Curiously, there is no corresponding set-entry-min-columns-visible function yet.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-entry)))

(defhelp set-entry-place-holder
    (use "(set-entry-place-holder entry str)")
  (info "Set the place holder string of #entry to #str. This is displayed as a prompt when no text is entered.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-entry set-entry-text)))

(defhelp set-entry-text
    (use "(set-entry-text entry str)")
  (info "Set the text of #entry to string #str.")
  (type proc)
  (arity 2)
  (topic (gui))
  (see (new-entry set-entry-place-holder)))
