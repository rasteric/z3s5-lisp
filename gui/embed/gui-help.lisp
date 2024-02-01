;;;; GUI help definitions
;;;; This file is embedded and loaded with function `DefGUIHelp(*z3.Interp)`.

(set-help-topic-info 'gui
		     "Graphical User Interface"
		     "This section describes the GUI-related functions. These functions are only available when Z3S5 Lisp has been compiled with the embedded GUI package. See the `z3g` executable defined in `cmd/z3g/z3g.go` for an example of how to include the GUI and start it up. The key is that the interpreter must run in its own goroutine because the GUI is blocking once it has been called. GUI functions are threadsafe, or at least as threadsafe as the underlying GUI framework. Functions defined in Lisp are derived from corresponding functions of the `Fyne` framework and listed under the 'gui label in this help system. The naming conventions for translation between Go and Lisp functions are as follows:\n\n1. Camelcase is translated to lowercase with hyphens.\n2. A function `object.VerbQualifier` becomes verb-object-qualifier.\n3. Getters are written in the form `get-object-qualifier` and setters `set-object-qualifier`.\n4. As an exception of the previous rules, when the result of a function is a bool, the form is `object-predicate?`.\n\nFyne objects are represented by integer numbers. The system internally translates between these numbers and objects. Occasionally, Fyne objects are created on the fly for performance reasons. For example, sometimes color lists of the form `(r g b a)` with integers `r`, `g`,`b`, `a` are used instead of creating and storing color objects using `(nrgba r g b a)`. There are also sometimes shortcut accessors using selector symbols and other convenience wrappers for Fyne functions. When in doubt, refer to the Lisp help for details.\n\nWhen importing the GUI with `DefGUI`, a `Config` structure is provided that allows for restricted security. This makes it possible to use the GUI functions in a restricted environment that e.g. does not allow the creation of new windows.")

(defhelp new-window
    (use "(new-window title) => int")
  (info "Create a new window with #title string and return the window ID. This function raises an error if the host configuration WindowsAllowed is not true. In certain embedded uses, creating new windows is not allowed and you should check the documentation how to find a pre-configured window and add user interface elements to it.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (set-window-content close-window show-window)))

(defhelp set-window-content
    (use "(set-window-content window canvas-object)")
  (info "Set the main content of the window, which must be an existing canvas object such as a widget or container with layout.")
  (type proc)
  (arity 2)
  (topic (gui window))
  (see (get-window-content get-window-canvas new-window)))

(defhelp get-window-content
    (use "(get-window-content window) => int")
  (info "Get the canvas object ID that represents the main content of the window. This is usually a widget or a container with layout.")
  (type proc)
  (arity 2)
  (topic (gui window))
  (see (set-window-content get-window-canvas)))

(defhelp set-window-size
    (use "(set-window-size window width height)")
  (info "Set the size of #window to #width and #height as floats. Sizes and positions are generally given as floats whose accuracy is only guaranteed to 32 bit.")
  (type proc)
  (arity 2)
  (topic (gui window))
  (see (new-window show-window hide-window)))

(defhelp close-window
    (use "(close-window window)")
  (info "Closes #window and delete it from internal storage. This frees window resources. It cannot be re-used after this operation. Use window-hide if you want to close/hide a window only temporarily. Notice that unlike in Fyne, there is no mechanism to close an application automatically after its main window has been closed.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (hide-window)))

(defhelp hide-window
    (use "(hide-window window)")
  (info "Hides #window. It can be shown again using show-window.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (show-window close-window)))

(defhelp set-window-on-close-callback
    (use "(set-window-on-close-callback window proc)")
  (info "Sets the close callback of #window. #proc must be a function that takes no arguments and is evaluated when the window is closed.")
  (type proc)
  (arity 2)
  (topic (gui window))
  (see (show-window close-window hide-window)))

(defhelp get-window-canvas
    (use "(get-window-canvas window) => int")
  (info "Get the canvas object of #window, which is the area on which window elements are drawn. This is not the same as the window-content, which is a widget or other user interface element. The canvas is used for raw-drawing commands, for example for drawing circles and boxes. With a suitable layout that doesn't re-arrange objects, it can e.g. be used to draw overlays.")
  (type proc)
  (arity 1)
  (topic (gui window drawing))
  (see (get-window-content set-window-content focus-canvas-object)))

(defhelp get-window-title
    (use "(get-window-title window) => str")
  (info "Return the title of #window as string.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (set-window-title)))

(defhelp set-window-title
    (use "(set-window-title window title)")
  (info "Set the title of #window to string #title.")
  (type proc)
  (arity 2)
  (topic (gui window))
  (see (get-window-title)))

(defhelp set-window-full-screen
    (use "(set-window-full-screen window full-screen?)")
  (info "If #full-screen? is not nil, then #window is set to full screen mode, otherwise the window is set to normal mode. In full screen mode the window is centered and fills the whole screen of the main monitor (multiple monitors are currently not supported).")
  (type proc)
  (arity 2)
  (topic (gui window drawing))
  (see (window-full-screen? center-window-on-screen)))

(defhelp window-full-screen?
    (use "(window-full-screen? window) => bool")
  (info "Return nil if #window is full screen, true otherwise.")
  (type proc)
  (arity 1)
  (topic (gui window drawing))
  (see (set-window-full-screen center-window-on-screen)))

(defhelp set-window-fixed-size
    (use "(set-window-fixed-size window fixed-size?)")
  (info "If #fixed-size? is not nil, then #window is set to fixed size, i.e., it has no resize button and cannot be resized by the user; otherwise, the window is set to being resizable.")
  (type proc)
  (arity 2)
  (topic (gui window))
  (see (window-fixed-size?)))

(defhelp window-fixed-size?
    (use "(window-fixed-size? window) => bool")
  (info "Return nil if #window is fixed size, true otherwise.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (set-window-fixed-size)))

(defhelp center-window-on-screen
    (use "(center-window-on-screen window)")
  (info "As the name implies, this function centers the window on the screen.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (set-window-full-screen window-full-screen?)))

(defhelp set-window-padded
    (use "(set-window-padded window padded?)")
  (info "If #padded? is not nil, then #window is set to being padded. This is the default of new window. If #padded? is nil, then the window's padding is removed, which means that the whole content area of the window can be filled with user interface elements and draw commands. This would be used for a game display, for instance.")
  (type proc)
  (arity 2)
  (topic (gui window drawing))
  (see (window-padded? set-window-full-screen window-full-screen? center-window-on-screen)))

(defhelp window-padded?
    (use "(window-padded? window) => bool")
  (info "Return nil if #window is padded, true otherwise.")
  (type proc)
  (arity 1)
  (topic (gui window drawing))
  (see (set-window-padded set-window-full-screen center-window-on-screen)))

(defhelp set-window-icon
    (use "(set-window-icon window resource)")
  (info "Set the icon of #window to the given icon #resource. Setting the icon does not guarantee that it is displayed, since this is platform-dependent.")
  (type proc)
  (arity 1)
  (topic (gui window))
  (see (get-window-icon new-icon theme-icon)))

(defhelp get-window-icon
    (use "(get-window-icon window) => int")
  (info "Obtain the icon ID of the icon of #window. The resource obtained is not guaranteed to be a visible icon or might be a dummy, as not all windows have icons on all platforms.")
  (type proc)
  (arity 1)
  (topic (gui window))
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
  (topic (gui window menu))
  (see (set-window-main-menu new-main-menu)))

(defhelp new-label
    (use "(new-label str) => int")
  (info "Creates a new text label with string #str.")
  (type proc)
  (arity 1)
  (topic (gui label))
  (see (set-label-text)))

(defhelp set-label-text
    (use "(set-label-text label str)")
  (info "Sets the text of #label to string #str. This might resize the label depending on the layout in which the label is put.")
  (type proc)
  (arity 2)
  (topic (gui label))
  (see (get-label-text new-label)))

(defhelp get-label-text
     (use "(get-label-text label) => str")
  (info "Gets the text of #label")
  (type proc)
  (arity 1)
  (topic (gui label))
  (see (set-label-text new-label)))

(defhelp new-entry
    (use "(new-entry [selector]) => int")
  (info "Create a new text entry field based on the optional #selector symbol. #selector can be a symbol in '(single-line multi-line password). The default is 'single-line.")
  (type proc)
  (arity 1)
  (topic (gui entry))
  (see (set-entry-on-change-callback set-entry-validator entry-accepts-tab? get-entry-cursor-pos set-entry-cursor-row set-entry-cursor-column
				     set-entry-on-cursor-change-callback get-entry-cursor get-entry-selected-text set-entry-min-rows-visible
				     set-entry-place-holder set-entry-text)))

(defhelp set-entry-on-change-callback
    (use "(set-entry-on-change-callback entry proc)")
  (info "Set the callback of #entry that is triggered when the entry text changes. #proc must be a procedure that takes the entry text as string.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (new-entry set-entry-cursor-change-callback)))

(defhelp set-entry-validator
    (use "(set-entry-validator entry validator)")
  (info "Set the #validator of #entry. A validator must be created first from a special procedure or a regular expression.")
  (type proc)
  (arity 2)
  (topic (gui entry validation))
  (see (new-entry new-validator new-combined-string-validator new-time-validator new-regexp-validator validate-object)))

(defhelp entry-accepts-tab?
    (use "(entry-accepts-tab? entry) => bool")
  (info "Return #nil when the entry does not accept tabs, #t otherwise.")
  (type proc)
  (arity 1)
  (topic (gui entry))
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
  (topic (gui entry))
  (see (get-entry-cursor-pos set-entry-cursor-column)))

(defhelp set-entry-cursor-column
    (use "(set-entry-cursor-column entry column)")
  (info "Set the column position of the cursor in #entry to integer #column.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (get-entry-cursor-pos set-entry-cursor-row)))

(defhelp set-entry-on-cursor-change-callback
    (use "(set-entry-cursor-change-callback entry proc)")
  (info "Set the cursor change callback of #entry to #proc, which is a procedure that takes the entry ID as argument.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (new-entry set-entry-on-change-callback)))

(defhelp get-entry-cursor
    (use "(get-entry-cursor entry) => sym")
  (info "Return a symbol that represents the current cursor of #entry. Possible values are in '(default text crosshair pointer hresize vresize). Curiously, there is no way to set the cursor yet.")
  (type proc)
  (arity 1)
  (topic (gui entry))
  (see (new-entry)))

(defhelp set-entry-min-rows-visible
    (use "(set-entry-min-rows-visible entry rows)")
  (info "Set the minimum number of rows of #entry that are visible. This ensures that #rows text rows are visible and is a way of setting the entry's minimum size. Curiously, there is no corresponding set-entry-min-columns-visible function yet.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (new-entry)))

(defhelp set-entry-place-holder
    (use "(set-entry-place-holder entry str)")
  (info "Set the place holder string of #entry to #str. This is displayed as a prompt when no text is entered.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (new-entry set-entry-text)))

(defhelp set-entry-text
    (use "(set-entry-text entry str)")
  (info "Set the text of #entry to string #str.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (new-entry set-entry-place-holder)))

(defhelp new-combined-string-validator
    (use "(new-combined-string-validator validator-1 [...validator-n]) => int")
  (info "Combine validators #validator-1 to #validator-n into a combined string validator and return it.")
  (type proc)
  (arity -2)
  (topic (gui validation))
  (see (set-entry-validator new-validator new-regexp-validator new-time-validator set-object-on-validation-change-callback validate-object)))

(defhelp new-regexp-validator
    (use "(new-regexp-validator regexp reason) => int")
  (info "Create a new string validator from the #regexp string, which must be a valid regular expression in Go's regexp syntax. The #reason string is displayed to the user by widgets like #entry when the validation fails.")
  (type proc)
  (arity 2)
  (topic (gui validation))
  (see (set-entry-validator new-validator new-combined-string-validator new-time-validator set-object-on-validation-change-callback validate-object)))

(defhelp new-time-validator
    (use "(new-time-validator format-str) => int")
  (info "Create a new string validator for time and date based on the given template #format-str. This validator uses Go's data parsing function and therefore is quite restrictive. Only datetimes entered in exactly the format given (including timezones) validate successfully. To obtain a more relaxed date and time validator, use #new-validator to create a custom validator with your own parsing or try a #new-regexp-validator.")
  (type proc)
  (arity 1)
  (topic (gui validation))
  (see (set-entry-validator new-validator new-combined-string-validator new-time-validator new-regexp-validator set-object-on-validation-change-callback validate-object)))

(defhelp new-validator
    (use "(new-validator proc) => int")
  (info "Create a new string validator based on validation procedure #proc. The procedure #proc takes a string as argument and returns a string. If the string returned is not the empty string \"\", then validation fails and the returned string is given as a reason for validation failure. If the empty string is returned, then validation succeeds. If an error occurs in #proc, then validation fails with the error's error message as reason. Notice that validators are fairly limited and can only be attached to a few validatable objects such as text entry fields. For a more general approach, it might make sense to implement your own validation system based on key press, focus change, and change callbacks of various GUI objects.")
  (type proc)
  (arity 1)
  (topic (gui validation))
  (see (set-entry-validator new-combined-string-validator new-regexp-validator new-time-validator set-object-on-validation-change-callback validate-object)))

(defhelp set-object-on-validation-change-callback
    (use "(set-object-on-validation-change-callback obj proc)")
  (info "Set a validatable object's #obj validation change callback, which is called when the object's validation changes. The callback #proc takes a string or nil as argument. When it is nil, the validation was successful. When it is a string, then the validation failed with the string as reason. This can be used to track validation changes of any validatable object (such as a text entry) to e.g. display custom messages or icons when the validation fails or succeeds.")
  (type proc)
  (arity 2)
  (topic (gui validation))
  (see (validate-object new-validator set-entry-validator)))

(defhelp validate-object
    (use "(validate-object obj) => str")
  (info "Validate the validatable object #obj programmatically and return the validation failure as string, or the empty string if validation succeeded. It sometimes makes sense to call this explicitly in order to force the object to display its validation state.")
  (type proc)
  (arity 2)
  (topic (gui validation))
  (see (set-object-on-validation-change-callback new-validator set-entry-validator)))

(defhelp new-text-grid
    (use "(new-text-grid [<string>] [show-line-numbers|show-whitespace|tab-width <int>]) => int")
  (info "Create a new text grid widget, which displays multiline text with custom background and foreground colors. The optional #string argument is the initial text of the grid without formatting. The following symbols might be #'show-line-numbers to turn the line number display on and #'show-whitespace to display white space characters by special unicode symbols. If the selector #'tab-width occurs, then it must be immediately followed by an integer for the tabulator width of the text grid in space characters.")
  (type proc)
  (arity -1)
  (topic (gui text-grid))
  (see (new-zgrid text-grid-show-line-numbers? text-grid-show-whitespace? get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace get-text-grid-row get-text-grid-row-text
		  set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		  get-text-grid-text remove-text-grid-row insert-text-grid-row)))

(defhelp get-text-grid-cell-size
    (use "(get-text-grid-cell-size grid) => li")
  (info "Return the size of one text grid cell as a list of floats (w h) where #w is the width and #h is the height.")
  (type proc)
  (arity 1)
  (topic (gui text-grid))
  (see (new-text-grid)))

(defhelp text-grid-show-line-numbers?
    (use "(text-grid-show-line-numbers? grid) => bool")
  (info "Return true if the text #grid shows line numbers, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui text-grid))
  (see (new-text-grid text-grid-show-whitespace? get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace get-text-grid-row get-text-grid-row-text
		      set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		      get-text-grid-text)))

(defhelp text-grid-show-whitespace?
    (use "(text-grid-show-whitespace? grid) => bool")
  (info "Return true if the text #grid shows whitespace glyphs, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui text-grid))
  (see (new-text-grid text-grid-show-line-numbers? get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace get-text-grid-row get-text-grid-row-text
		      set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		      get-text-grid-text)))

(defhelp get-text-grid-tab-width
    (use "(get-text-grid-tab-width grid) => int")
  (info "Return the current tabulator width of #grid in space characters.")
  (type proc)
  (arity 1)
  (topic (gui text-grid))
  (see (new-text-grid text-grid-show-line-numbers? text-grid-show-whitespace? set-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace get-text-grid-row get-text-grid-row-text
		      set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		      get-text-grid-text)))

(defhelp set-text-grid-tab-width
    (use "(set-text-grid-tab-width grid width)")
  (info "Set the tabulator width of #grid to integer #width space characters.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (new-text-grid text-grid-show-line-numbers? text-grid-show-whitespace? get-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace get-text-grid-row get-text-grid-row-text
		      set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		      get-text-grid-text)))

(defhelp set-text-grid-show-line-numbers
    (use "(set-text-grid-show-line-numbers grid show?)")
  (info "Set whether #grid shows line numbers. If #show? is not nil, then line numbers are shown, otherwise they are not shown.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (new-text-grid text-grid-show-line-numbers? text-grid-show-whitespace? get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-whitespace get-text-grid-row get-text-grid-row-text
		      set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		      get-text-grid-text)))

(defhelp set-text-grid-show-whitespace
    (use "(set-text-grid-show-whitespace grid show?)")
  (info "Set whether #grid shows whitespace characters. If #show? is not nil, then whitespace characters are shown, otherwise they are not shown.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (new-text-grid text-grid-show-line-numbers? text-grid-show-whitespace? get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-line-numbers get-text-grid-row get-text-grid-row-text
		      set-text-grid-cell get-text-grid-cell set-text-grid-row set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
		      get-text-grid-text)))

(defhelp get-text-grid-row
    (use "(get-text-grid-row grid row) => li")
  (info "Obtain a #row of a text #grid, where #row is a 0-based index. This function returns a list of the form '(row style), where #style is a grid style list and #row is an array of lists consisting each of a unicode string containing one rune and a grid style list. Each entry of the #row array represents an individual unicode glyph with a style, whereas the #style list in the return argument represents an optional style of the whole row.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see  (set-text-grid-row get-text-grid-row-text get-text-grid-cell new-text-grid text-grid-show-line-numbers? text-grid-show-whitespace?
			   get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace  
			   set-text-grid-cell  set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
			   get-text-grid-text remove-text-grid-row insert-text-grid-row)))

(defhelp get-text-grid-row-text
    (use "(get-text-grid-row-text grid row) => str")
  (info "Return the text of #row in #grid as a string without any style information.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see  (set-text-grid-rune  get-text-grid-row get-text-grid-cell set-text-grid-row new-text-grid text-grid-show-line-numbers? text-grid-show-whitespace?
			     get-text-grid-tab-width set-text-grid-tab-width set-text-grid-show-line-numbers set-text-grid-show-whitespace  
			     set-text-grid-cell  set-text-grid-row-style set-text-grid-rune set-text-grid-style set-text-grid-style-range set-text-grid-text
			     get-text-grid-text)))

(defhelp set-text-grid-cell
    (use "(set-text-grid-cell grid row column li)")
  (info "Set the text #grid cell at #row and #column (both 0-indexed) to the list #li, where #li must consist of a unicode string containing one rune and a valid grid style list.")
  (type proc)
  (arity 4)
  (topic (gui text-grid))
  (see (get-text-grid-cell set-text-grid-rune get-text-grid-row set-text-grid-row)))


(defhelp get-text-grid-cell
    (use "(get-text-grid-cell grid row column) => li")
  (info "Return the cell of #grid at #row and #column. The result is a list consisting of a string containing one unicode rune and a grid style list. The style might be nil. If it is not nil, then the list contains a foreground and a background color list.")
  (type proc)
  (arity 3)
  (topic (gui text-grid))
  (see (get-text-grid-rune set-text-grid-cell get-text-grid-row set-text-grid-rune set-text-grid-style-range get-text-grid-style)))

(defhelp remove-text-grid-row
    (use "(remove-text-grid-row grid row)")
  (info "Remove the #row from the given text #grid. An incorrect #row index will result in an error.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (insert-text-grid-row new-text-grid get-text-grid-row)))

(defhelp insert-text-grid-row
    (use "(insert-text-grid-row grid row)")
  (info "Insert a new text grid row before #row in the given text #grid. If #row is the number of rows, a new row is appended to the end of the text grid.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (remove-text-grid-row count-text-grid-rows new-text-grid get-text-grid-row)))

(defhelp get-text-grid-rune
    (use "(get-text-grid-rune grid row column) => str")
  (info "Return the string containing a single rune at the cell in #row and #column of #grid.")
  (type proc)
  (arity 3)
  (topic (gui text-grid))
  (see (get-text-grid-cell get-text-grid-style get-text-grid-row)))

(defhelp count-text-grid-rows
    (use "(count-text-grid-rows grid) => int")
  (info "Return the number of the last row in grid, 0 if there are none.")
  (type proc)
  (arity 1)
  (topic (gui text-grid))
  (see (count-text-grid-row-columns get-text-grid-cell get-text-grid-row)))

(defhelp count-text-grid-row-columns
    (use "(count-text-grid-row-columns grid row) => int")
  (info "Return the number of columns in #row of #grid, 0 if there are none.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (count-text-grid-rows get-text-grid-cell get-text-grid-row)))

(defhelp set-text-grid-row
    (use "(set-text-grid-row grid row row-spec)")
  (info "Set the #row of #grid to the given #row-spec, which is a list containing an array of grid cells like in the return value of #get-text-grid-row and a grid style for the row as a whole.")
  (type proc)
  (arity 3)
  (topic (gui text-grid))
  (see (get-text-grid-row set-text-grid-row-style set-text-grid-cell set-text-grid-rune set-text-grid-style-range)))

(defhelp set-text-grid-row-style
    (use "(set-text-grid-row-style grid row style)")
  (info "Set the style of text #grid at #row to the given grid #style.")
  (type proc)
  (arity 3)
  (topic (gui text-grid))
  (see (set-text-grid-row set-text-grid-cell get-text-grid-row set-text-grid-rune set-text-grid-style-range)))

(defhelp set-text-grid-rune
    (use "(set-text-grid-rune grid row column str)")
  (info "Set the rune of #grid at #row and #column to the unicode glyph in string #str.")
  (type proc)
  (arity 4)
  (topic (gui text-grid))
  (see (set-text-grid-style set-text-grid-cell get-text-grid-cell)))

(defhelp set-text-grid-style
    (use "(set-text-grid-style grid row column style)")
  (info "Set the grid style of #grid at #row and #column to the list #style.")
  (type proc)
  (arity 4)
  (topic (gui text-grid))
  (see (set-text-grid-cell set-text-grid-rune set-text-grid-style-range)))

(defhelp set-text-grid-style-range
    (use "(set-text-grid-style-range grid start-row start-column end-row end-column style)")
  (info "Set the grid style of #grid starting at #start-row and #start-column and ending at #end-row and #end-column (all inclusive) to the grid #style.")
  (type proc)
  (arity 6)
  (topic (gui text-grid))
  (see (set-text-grid-style set-text-grid-cell set-text-grid-row-style)))

(defhelp set-text-grid-text
    (use "(set-text-grid-text grid str)")
  (info "Set the text of the text #grid to the given #str.")
  (type proc)
  (arity 2)
  (topic (gui text-grid))
  (see (get-text-grid-text new-text-grid set-text-grid-rune set-text-grid-row)))

(defhelp get-text-grid-text
    (use "(get-text-grid-text grid) => str")
  (info "Return the text of #grid as a string without style information.")
  (type proc)
  (arity 1)
  (topic (gui text-grid))
  (see (set-text-grid-text new-text-grid get-text-grid-row get-text-grid-rune get-text-grid-cell)))

(defhelp wrap-delete-text-grid
    (use "(wrap-delete-text-grid grid range-list wrapcol soft-wrap? hard-lf-rune soft-lf-rune cursor-row cursor-column) => li")
  (info "This helper implements deletion with word wrapping in #grid. The #range-list must contain integers of the form (start-row start-colum end-row end-column), which must be within the grid's maximum row and column ranges. #wrapcol is an integer indicating the number of chars per line; any more chars are wrapped. If #soft-wrap? is not nil, then the paragraphs in which deletion takes place are soft-wrapped. #hard-lf-rune is a string containing the rune for a hard line feed, whereas #soft-lf-rune is a string containing the rune for soft line feeds. The current #cursor-row and #cursor-column must be provided as well; when the function wraps the deleted paragraphs, their values are updated and returned in the list #li, which is of the form (new-cursor-row new-cursor-column).")
  (type proc)
  (arity 8)
  (topic (gui text-grid))
  (see (wrap-insert-text-grid new-text-grid)))

(defhelp wrap-insert-text-grid
    (use "(wrap-insert-text-grid grid cells row col wrapcol soft-wrap? hard-lf-rune soft-lf-rune) => li")
  (info "This helper implements inserting styled text with word wrapping in #grid. #cells must be a list of text grid cells, each of which consists of a rune string, and a list containing a foreground and background color, or nil. #row and #col are the line and column in #grid before which the text is inserted. The number of characters per line is indicated with #wrapcol. If #soft-wrap? is true, then the paragraph into which it is inserted is soft-word-wrapped, using soft-lf-rune as a line ending. Otherwise, #hard-lf-rune is used for line-endings, which is also used for the last line of a paragraph. The returned list of the form (new-cursor-row new-cursor-column) reflects the updated cursor position if #row and #col are the current cursor position.")
  (type proc)
  (arity 8)
  (topic (gui text-grid))
  (see (wrap-delete-text-grid new-text-grid)))

(defhelp new-zgrid
    (use "(new-zgrid columns lines [<string>] [show-line-numbers|show-whitespace|tab-width <int>]) => int")
  (info "Create a new extended text grid widget called a zgrid. This widget is similar to text-grid and has most of its methods, but additionally displays a vertical scroll bar, handles many lines more efficiently than text-grid (though still sometimes slow), and provides a number of other extensions for syntax coloring and tagging of arbitrary text ranges. The optional #string argument is the initial text of the grid without formatting. The following symbols might be #'show-line-numbers to turn the line number display on and #'show-whitespace to display white space characters by special unicode symbols. If the selector #'tab-width occurs, then it must be immediately followed by an integer for the tabulator width of the zgrid in space characters.")
  (type proc)
  (arity -1)
  (topic (gui zgrid))
  (see (zgrid-show-line-numbers? zgrid-show-whitespace? get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace get-zgrid-row get-zgrid-row-text
				     set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
				     get-zgrid-text remove-zgrid-row insert-zgrid-row)))

(defhelp get-zgrid-cell-size
    (use "(get-zgrid-cell-size grid) => li")
  (info "Return the size of one zgrid cell as a list of floats (w h) where #w is the width and #h is the height.")
  (type proc)
  (arity 1)
  (topic (gui zgrid))
  (see (new-zgrid)))

(defhelp zgrid-show-line-numbers?
    (use "(zgrid-show-line-numbers? grid) => bool")
  (info "Return true if the text #grid shows line numbers, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui zgrid))
  (see (new-zgrid zgrid-show-whitespace? get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace get-zgrid-row get-zgrid-row-text
		      set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
		      get-zgrid-text)))

(defhelp zgrid-show-whitespace?
    (use "(zgrid-show-whitespace? grid) => bool")
  (info "Return true if the text #grid shows whitespace glyphs, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui zgrid))
  (see (new-zgrid zgrid-show-line-numbers? get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace get-zgrid-row get-zgrid-row-text
		      set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
		      get-zgrid-text)))

(defhelp get-zgrid-tab-width
    (use "(get-zgrid-tab-width grid) => int")
  (info "Return the current tabulator width of #grid in space characters.")
  (type proc)
  (arity 1)
  (topic (gui zgrid))
  (see (new-zgrid zgrid-show-line-numbers? zgrid-show-whitespace? set-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace get-zgrid-row get-zgrid-row-text
		      set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
		      get-zgrid-text)))

(defhelp set-zgrid-tab-width
    (use "(set-zgrid-tab-width grid width)")
  (info "Set the tabulator width of #grid to integer #width space characters.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (new-zgrid zgrid-show-line-numbers? zgrid-show-whitespace? get-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace get-zgrid-row get-zgrid-row-text
		      set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
		      get-zgrid-text)))

(defhelp set-zgrid-show-line-numbers
    (use "(set-zgrid-show-line-numbers grid show?)")
  (info "Set whether #grid shows line numbers. If #show? is not nil, then line numbers are shown, otherwise they are not shown.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (new-zgrid zgrid-show-line-numbers? zgrid-show-whitespace? get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-whitespace get-zgrid-row get-zgrid-row-text
		      set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
		      get-zgrid-text)))

(defhelp set-zgrid-show-whitespace
    (use "(set-zgrid-show-whitespace grid show?)")
  (info "Set whether #grid shows whitespace characters. If #show? is not nil, then whitespace characters are shown, otherwise they are not shown.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (new-zgrid zgrid-show-line-numbers? zgrid-show-whitespace? get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-line-numbers get-zgrid-row get-zgrid-row-text
		      set-zgrid-cell get-zgrid-cell set-zgrid-row set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
		      get-zgrid-text)))

(defhelp get-zgrid-row
    (use "(get-zgrid-row grid row) => li")
  (info "Obtain a #row of a text #grid, where #row is a 0-based index. This function returns a list of the form '(row style), where #style is a grid style list and #row is an array of lists consisting each of a unicode string containing one rune and a grid style list. Each entry of the #row array represents an individual unicode glyph with a style, whereas the #style list in the return argument represents an optional style of the whole row.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see  (set-zgrid-row get-zgrid-row-text get-zgrid-cell new-zgrid zgrid-show-line-numbers? zgrid-show-whitespace?
			   get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace  
			   set-zgrid-cell  set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
			   get-zgrid-text remove-zgrid-row insert-zgrid-row)))

(defhelp get-zgrid-row-text
    (use "(get-zgrid-row-text grid row) => str")
  (info "Return the text of #row in #grid as a string without any style information.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see  (set-zgrid-rune  get-zgrid-row get-zgrid-cell set-zgrid-row new-zgrid zgrid-show-line-numbers? zgrid-show-whitespace?
			     get-zgrid-tab-width set-zgrid-tab-width set-zgrid-show-line-numbers set-zgrid-show-whitespace  
			     set-zgrid-cell  set-zgrid-row-style set-zgrid-rune set-zgrid-style set-zgrid-style-range set-zgrid-text
			     get-zgrid-text)))

(defhelp set-zgrid-cell
    (use "(set-zgrid-cell grid row column li)")
  (info "Set the text #grid cell at #row and #column (both 0-indexed) to the list #li, where #li must consist of a unicode string containing one rune and a valid grid style list.")
  (type proc)
  (arity 4)
  (topic (gui zgrid))
  (see (get-zgrid-cell set-zgrid-rune get-zgrid-row set-zgrid-row)))


(defhelp get-zgrid-cell
    (use "(get-zgrid-cell grid row column) => li")
  (info "Return the cell of #grid at #row and #column. The result is a list consisting of a string containing one unicode rune and a grid style list. The style might be nil. If it is not nil, then the list contains a foreground and a background color list.")
  (type proc)
  (arity 3)
  (topic (gui zgrid))
  (see (get-zgrid-rune set-zgrid-cell get-zgrid-row set-zgrid-rune set-zgrid-style-range get-zgrid-style)))

(defhelp remove-zgrid-row
    (use "(remove-zgrid-row grid row)")
  (info "Remove the #row from the given text #grid. An incorrect #row index will result in an error.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (insert-zgrid-row new-zgrid get-zgrid-row)))

(defhelp insert-zgrid-row
    (use "(insert-zgrid-row grid row)")
  (info "Insert a new zgrid row before #row in the given text #grid. If #row is the number of rows, a new row is appended to the end of the zgrid.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (remove-zgrid-row count-zgrid-rows new-zgrid get-zgrid-row)))

(defhelp get-zgrid-rune
    (use "(get-zgrid-rune grid row column) => str")
  (info "Return the string containing a single rune at the cell in #row and #column of #grid.")
  (type proc)
  (arity 3)
  (topic (gui zgrid))
  (see (get-zgrid-cell get-zgrid-style get-zgrid-row)))

(defhelp count-zgrid-rows
    (use "(count-zgrid-rows grid) => int")
  (info "Return the number of the last row in grid, 0 if there are none.")
  (type proc)
  (arity 1)
  (topic (gui zgrid))
  (see (count-zgrid-row-columns get-zgrid-cell get-zgrid-row)))

(defhelp count-zgrid-row-columns
    (use "(count-zgrid-row-columns grid row) => int")
  (info "Return the number of columns in #row of #grid, 0 if there are none.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (count-zgrid-rows get-zgrid-cell get-zgrid-row)))

(defhelp set-zgrid-row
    (use "(set-zgrid-row grid row row-spec)")
  (info "Set the #row of #grid to the given #row-spec, which is a list containing an array of grid cells like in the return value of #get-zgrid-row and a grid style for the row as a whole.")
  (type proc)
  (arity 3)
  (topic (gui zgrid))
  (see (get-zgrid-row set-zgrid-row-style set-zgrid-cell set-zgrid-rune set-zgrid-style-range)))

(defhelp set-zgrid-row-style
    (use "(set-zgrid-row-style grid row style)")
  (info "Set the style of text #grid at #row to the given grid #style.")
  (type proc)
  (arity 3)
  (topic (gui zgrid))
  (see (set-zgrid-row set-zgrid-cell get-zgrid-row set-zgrid-rune set-zgrid-style-range)))

(defhelp set-zgrid-rune
    (use "(set-zgrid-rune grid row column str)")
  (info "Set the rune of #grid at #row and #column to the unicode glyph in string #str.")
  (type proc)
  (arity 4)
  (topic (gui zgrid))
  (see (set-zgrid-style set-zgrid-cell get-zgrid-cell)))

(defhelp set-zgrid-style
    (use "(set-zgrid-style grid row column style)")
  (info "Set the grid style of #grid at #row and #column to the list #style.")
  (type proc)
  (arity 4)
  (topic (gui zgrid))
  (see (set-zgrid-cell set-zgrid-rune set-zgrid-style-range)))

(defhelp set-zgrid-style-range
    (use "(set-zgrid-style-range grid start-row start-column end-row end-column style)")
  (info "Set the grid style of #grid starting at #start-row and #start-column and ending at #end-row and #end-column (all inclusive) to the grid #style.")
  (type proc)
  (arity 6)
  (topic (gui zgrid))
  (see (set-zgrid-style set-zgrid-cell set-zgrid-row-style)))

(defhelp set-zgrid-text
    (use "(set-zgrid-text grid str)")
  (info "Set the text of the text #grid to the given #str.")
  (type proc)
  (arity 2)
  (topic (gui zgrid))
  (see (get-zgrid-text new-zgrid set-zgrid-rune set-zgrid-row)))

(defhelp get-zgrid-text
    (use "(get-zgrid-text grid) => str")
  (info "Return the text of #grid as a string without style information.")
  (type proc)
  (arity 1)
  (topic (gui zgrid))
  (see (set-zgrid-text new-zgrid get-zgrid-row get-zgrid-rune get-zgrid-cell)))

(defhelp wrap-delete-zgrid
    (use "(wrap-delete-zgrid grid range-list wrapcol soft-wrap? hard-lf-rune soft-lf-rune cursor-row cursor-column) => li")
  (info "This helper implements deletion with word wrapping in #grid. The #range-list must contain integers of the form (start-row start-colum end-row end-column), which must be within the grid's maximum row and column ranges. #wrapcol is an integer indicating the number of chars per line; any more chars are wrapped. If #soft-wrap? is not nil, then the paragraphs in which deletion takes place are soft-wrapped. #hard-lf-rune is a string containing the rune for a hard line feed, whereas #soft-lf-rune is a string containing the rune for soft line feeds. The current #cursor-row and #cursor-column must be provided as well; when the function wraps the deleted paragraphs, their values are updated and returned in the list #li, which is of the form (new-cursor-row new-cursor-column).")
  (type proc)
  (arity 8)
  (topic (gui zgrid))
  (see (wrap-insert-zgrid new-zgrid)))

(defhelp wrap-insert-zgrid
    (use "(wrap-insert-zgrid grid cells row col wrapcol soft-wrap? hard-lf-rune soft-lf-rune) => li")
  (info "This helper implements inserting styled text with word wrapping in #grid. #cells must be a list of zgrid cells, each of which consists of a rune string, and a list containing a foreground and background color, or nil. #row and #col are the line and column in #grid before which the text is inserted. The number of characters per line is indicated with #wrapcol. If #soft-wrap? is true, then the paragraph into which it is inserted is soft-word-wrapped, using soft-lf-rune as a line ending. Otherwise, #hard-lf-rune is used for line-endings, which is also used for the last line of a paragraph. The returned list of the form (new-cursor-row new-cursor-column) reflects the updated cursor position if #row and #col are the current cursor position.")
  (type proc)
  (arity 8)
  (topic (gui zgrid))
  (see (wrap-delete-zgrid new-zgrid)))

(defhelp new-check
    (use "(new-check title proc) => int")
  (info "Create and return a new check box with the given #title string and a callback procedure #proc. The callback #proc is called with the new state of the check box as bool when it has changed.")
  (type proc)
  (arity 2)
  (topic (gui check))
  (see (new-choice)))

(defhelp new-choice
    (use "(new-choice selector string-list proc) => int")
  (info "Create and return a new choice representing choices in #string-list. If #selector is 'radio-group, a group of radio buttons is created with options in #string-list. If #selector is 'select, a more compact selection menu is created with the options in #string-list. The callback #proc takes a string that represents the choice that has been selected.")
  (type proc)
  (arity 3)
  (topic (gui choice))
  (see (new-check)))

(defhelp new-form
    (use "(new-form)")
  (info "Return a new form container, which orders widgets in rows, where each row has a label and a widget whose columns are aligned with the other rows. Use append-form to add label and widgets.")
  (type proc)
  (arity 0)
  (topic (gui form))
  (see (append-form)))

(defhelp append-form
    (use "(append-form form str canvas-object)")
  (info "Append a new row to the bottom #form consisting of a label #str aligned with a #canvas-object, which may be an entry, button, etc.")
  (type proc)
  (arity 0)
  (topic (gui form))
  (see (new-form)))

(defhelp new-hyperlink
    (use "(new-hyperlink label url) => int")
  (info "Create a new hyperlink with given #label string and an #url string as embedded link. A hyperlink looks like a label with link style; when it is clicked, the #url is called by the default operating system mechanism for opening urls. Using hyperlinks might be disallowed by the host system configuration HyperlinksAllowed and may be re-written by the host system using the CheckHyperlinks function. If HyperlinksAllowed is false in the active GUI config of the host, this function raises an error. It also parses the given #url and will raise an error if it does not represent a valid URL.")
  (type proc)
  (arity 2)
  (topic (gui hyperlink))
  (see (new-button new-label))
  (warn "Allowing the host to open hyperlinks usually launches a web browser and the details depend on the operating system. There is an added security risk!"))

(defhelp new-button
    (use "(new-button label proc) => int")
  (info "Return a new button with the given #label and without an icon. The callback #proc is called without arguments when the button is pressed or tapped.")
  (type proc)
  (arity 2)
  (topic (gui button))
  (see (new-button-with-icon new-hyperlink new-label)))

(defhelp new-button-with-icon
    (use "(new-button-with-icon label icon proc) => int")
  (info "Return a new button the given #label and #icon. The callback #proc is called without arguments when the button is pressed.")
  (type proc)
  (arity 3)
  (topic (gui button))
  (see (new-button new-icon theme-icon)))

(defhelp new-list
    (use "(new-list len-proc prep-proc update-proc) => int")
  (info "Create a new list display. A list consists of rows of simple items like labels. The #len-proc must be a procedure without arguments returning the length of the list as integer. The #prep-proc must be a procedure without arguments that returns a canvas object (i.e. a label or other widgets) representing a template for a single list item. The #update-proc must be a procedure that receives the ID of a canvas object (given by the template) and the 0-based list index as arguments. This procedure then should modify the canvas object with #ID to display the given list item at the index. See the GUI examples on how to use this function.")
  (type proc)
  (arity 3)
  (topic (gui list))
  (see (new-table new-tree)))

(defhelp new-table
    (use "(new-table len-proc prep-proc update-proc) => int")
  (info "Create a new table display. A table consists of a number of rows, each of which has a fixed number of columns such as labels. The #len-proc must be a procedure without arguments returning the length of the table as integer. The #prep-proc must be a procedure without arguments that returns a canvas object that represents the table row with updatable columns. The #update-proc takes the row, column, and ID of a canvas object and updates a table template with the right display for the table cell at #row and #column.")
  (type proc)
  (arity 3)
  (topic (gui table))
  (see (new-list new-tree)))

(defhelp new-tree
    (use "(new-tree child-uid-proc is-branch-proc create-node-proc update-node-proc) => int")
  (info "Create a new tree display. A tree displays nested branches and leaf nodes. The #child-uid-proc is a procedure that takes an id string as argument. If the string is empty, it should return a list of top-level branch uid strings. If the string is not empty, it represents an uid; the procedure should then return a list of all child id strings of that branch. This defines the tree's structure. All id strings must be unique to the tree. The #is-branch-proc takes an id string as argument and should return non-nil if the id represents a branch, nil if it has no children. The #create-node-proc takes a bool #b as argument and should return a branch template if #b is non-nil and a leaf template object if #b is nil. Finally, the #update-node-proc is a procedure that takes a node id string, a boolean that is true if the node is a branch, and a node template canvas-object as it is returned by #create-node-proc. The procedure should fill the template with the display values for the respective node id.")
  (type proc)
  (arity 4)
  (topic (gui tree))
  (see (new-list new-table)))

(defhelp new-menu-item
    (use "(new-menu-item str proc [selector...]) => int")
  (info "Create a new menu item with given label #str and callback #proc, which takes no arguments. The optional #selector symbol may be one of: 'is-quit - the item is the application Quit menu item (this is dealt with differently by operating system requirements), 'is-separator - the item is a menu item separator and the label string is ignored (redundent, use #new-menu-item-separator instead), 'disabled - the menu item is disabled, or 'checked - the menu item is checked.")
  (type proc)
  (arity -3)
  (topic (gui menu-item))
  (see (set-menu-item-checked menu-item-checked? set-menu-item-disabled menu-item-disabled? get-menu-item-label set-menu-item-label new-menu* new-menu new-menu-item-separator)))

(defhelp set-menu-item-checked
    (use "(set-menu-item-checked item checked?)")
  (info "Set the menu item check mark display if #checked? is non-nil, remove it otherwise.")
  (type proc)
  (arity 2)
  (topic (gui menu-item))
  (see (menu-item-checked? set-menu-item-disabled menu-item-disabled? get-menu-item-label set-menu-item-label new-menu* new-menu new-menu-item new-menu-item-separator)))

(defhelp menu-item-checked?
    (use "(menu-item-checked? item) => bool")
  (info "Return true if #item is currently checked, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui menu-item))
  (see (set-menu-item-checked set-menu-item-disabled menu-item-disabled? get-menu-item-label set-menu-item-label new-menu* new-menu new-menu-item new-menu-item-separator)))

(defhelp set-menu-item-disabled
    (use "(set-menu-item-disabled item disabled?)")
  (info "Disable the menu item if #disabled? is non-nil, enable it otherwise.")
  (type proc)
  (arity 2)
  (topic (gui menu-item))
  (see (menu-item-disabled? set-menu-item-checked menu-item-checked? get-menu-item-label set-menu-item-label new-menu* new-menu new-menu-item new-menu-item-separator)))

(defhelp menu-item-disabled?
    (use "(menu-item-disabled? item) => bool")
  (info "Return true if #item is currently disabled, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui menu-item))
  (see (set-menu-item-disabled set-menu-item-checked menu-item-checked? get-menu-item-label set-menu-item-label new-menu* new-menu new-menu-item new-menu-item-separator)))

(defhelp get-menu-item-label
    (use "(get-menu-item-label item) => str")
  (info "Return the current label of the given menu #item.")
  (type proc)
  (arity 1)
  (topic (gui menu-item))
  (see (set-menu-item-label set-menu-item-disabled menu-item-disabled set-menu-item-checked menu-item-checked? new-menu* new-menu new-menu-item new-menu-item-separator)))

(defhelp set-menu-item-label
    (use "(set-menu-item-label item str)")
  (info "Set the label of menu #item to #str.")
  (type proc)
  (arity 2)
  (topic (gui menu-item))
  (see (get-menu-item-label set-menu-item-disabled menu-item-disabled set-menu-item-checked menu-item-checked? new-menu* new-menu new-menu-item new-menu-item-separator)))

(defhelp new-menu-item-separator
    (use "(new-menu-item-separator) => int")
  (info "Return a new menu item separator, which is a menu item without callback and label that displays a separator between menu items in menus.")
  (type proc)
  (arity 0)
  (topic (gui menu-item))
  (see (new-menu-item)))

(defhelp new-menu*
    (use "(new-menu* label [item...]) => int")
  (info "Make a new abstract menu with given #label and arbitary menu items #item ... following. The starred function is used to define a menu but is not bound to any particular way of displaying it (popup-menu, normal menu, main menu). Use #new-menu and #new-main-menu to create visible menus and menu bars based on such abstract menus.")
  (type proc)
  (arity -2)
  (topic (gui menu))
  (see (refresh-menu* new-menu new-main-menu)))

(defhelp refresh-menu*
    (use "(refresh-menu* menu)")
  (info "Refresh the given #menu after a change was made that has a visual impact. This will refresh the menu widget in which this abstract menu occurs.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (refresh-main-menu new-menu*)))

(defhelp new-menu
    (use "(new-menu menu*) => int")
  (info "Create a new visible menu widget from the abstract #menu* created by new-menu*.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (new-menu* new-main-menu)))

(defhelp activate-menu-last-submenu
    (use "(activate-menu-last-submenu menu) => bool")
  (info "Find the last active menu item traversing through open submenus, and activate its submenu if one is found. Return true if a submenu was activated, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (deactivate-menu-last-submenu new-menu activate-menu-next activate-menu-previous)))

(defhelp activate-menu-next
    (use "(activate-menu-next menu)")
  (info "Activate the menu item following the currently active menu item, if there is any.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (activate-menu-previous new-menu activate-menu-last-submenu)))

(defhelp activate-menu-previous
    (use "(activate-menu-previous menu)")
  (info "Activate the menu item before the currently active menu item, if there is any.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (activate-menu-next new-menu activate-menu-last-submenu)))

(defhelp deactivate-menu-child
    (use "(deactivate-menu-child menu)")
  (info "Deactivate the currently active menu item and close its submenu if there is one.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (activate-menu-last-submenu activate-menu-next activate-menu-previous new-menu)))

(defhelp deactivate-menu-last-submenu
    (use "(deactivate-menu-last-submenu menu)")
  (info "Traverse the menu and deactivate the last open submenu found.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (activate-menu-last-submenu activate-menu-next activate-menu-previous new-menu)))

(defhelp trigger-menu-last
    (use "(trigger-menu-last menu)")
  (info "Find the last active menu or submenu item and trigger it.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (activate-menu-last-submenu activate-menu-next activate-menu-previous new-menu)))

(defhelp new-main-menu
    (use "(new-main-menu menu ...) => int")
  (info "Return a new main menu with the given menus. A main menu displays a menubar for a window on some desktop platforms but it may also be displayed in other ways.")
  (type proc)
  (arity -2)
  (topic (gui menu))
  (see (new-menu new-menu*)))

(defhelp refresh-main-menu
    (use "(refresh-main-menu main-menu)")
  (info "Refresh the given #main-menu display. This should be called after some submenus or menu items in the main menu have changed.")
  (type proc)
  (arity 1)
  (topic (gui menu))
  (see (new-main-menu refresh-menu*)))

(defhelp new-image-from-resource
    (use "(new-image-from-resource resource) => int")
  (info "Create and return a new image from the given #resource.")
  (type proc)
  (arity 1)
  (topic (gui image))
  (see (new-image-from-file theme-icon)))

(defhelp new-image-from-file
    (use "(new-image-from-file path) => int")
  (info "Create and return a new image from the image file at #path, which must be a PNG file.")
  (type proc)
  (arity 1)
  (topic (gui image))
  (see (new-image-from-resource)))

(defhelp nrgba
    (use "(nrgba red green blue alpha) => int")
  (info "Create an RGBA color where #red, #green, #blue, and #alpha are 8-bit uint integers, i.e., values between 0 and 255 (inclusive). Notice that some GUI functions require NRGBA color returned by this function, whereas others require a color list of int values '(red green blue alpha). This is for performance reasons, since it sometimes faster to convert a list to a color on-the-fly and sometimes more convenient to store pre-defined colors for later re-use.")
  (type proc)
  (arity 4)
  (topic (gui drawing))
  (see (nrgba64 theme-color new-rectangle new-circle new-line new-text)))

(defhelp nrgba64
    (use "(nrgba64 red green blue alpha) => int")
  (info "Create a 64-bit RGBA color where #red, #green, #blue, and #alpha are 16-bit uint integers, i.e., values between 0 and 65365 (inclusive). Notice that some GUI functions require NRGBA64 color returned by this function, whereas others require a color list of int values '(red green blue alpha). This is for performance reasons, since it sometimes faster to convert a list to a color on-the-fly and sometimes more convenient to store pre-defined colors for later re-use.")
  (type proc)
  (arity 4)
  (topic (gui drawing))
  (see (nrgba theme-color new-rectangle new-circle new-line new-text)))

(defhelp new-rectangle
    (use "(new-rectangle fill-color [width height] [position] [stroke-color] [stroke-width] [corner-radius]) => int")
  (info "Draw and return a rectangle with the given NRGBA #fill-color. The optional int #width and #height arguments set the width and height of the rectangle explicitly (otherwise they are 1). The optional #position argument must be a list of #x and #y coordinates as floats. The optional #stroke-color and #stroke-width arguments determine the color and width of the outline of the rectangle, and the optional #corner-radious defines how rounded the rectangle is. Notice that the rectangle's size and position can be set by the layout of the container, so to set it manually you need to make sure the underlying container has no layout that positions or resizes the rectangle.")
  (type proc)
  (arity -2)
  (topic (gui drawing))
  (see (new-circle new-line new-text)))

(defhelp new-circle
    (use "(new-circle fill-color [pos1] [pos2] [stroke-color] [stroke-width]) => int")
  (info "Draw and return a circle with the given NRGBA #fill-color. If the optional #pos1 and #pos2 position lists of #x and #y coordinates in floats are given , then the circle is drawn inside the rectangle defined by these positions. The optional #stroke-color and #stroke-width arguments determine the outline of the circle. Notice that circle's size and position may be set by the layout of the container, so to set these manually using #pos1 and #pos2 you need to make sure the underlying container has no such layout.")
  (type proc)
  (arity -2)
  (topic (gui drawing))
  (see (new-rectangle new-line-new-text)))

(defhelp new-line
    (use "(new-line fill-color [pos1] [pos2] [stroke-color] [stroke-width]) => int")
  (info "Draw and return a line with the given NRGBA #fill-color from optional position #pos1 to position #pos2, where these are lists of #x and #y coordinates as floats. The optional #stroke-color and #stroke-width determines the outer edges of the line.")
  (type proc)
  (arity -2)
  (topic (gui drawing))
  (see (new-cirlce new-rectangle new-text)))

(defhelp new-text
    (use "(new-text str color) => int")
  (info "Draw and return text with the given string #str and foreground NRGBA #color.")
  (arity 2)
  (type proc)
  (arity 2)
  (topic (gui drawing))
  (see (set-text-alignment set-text-size set-text-style new-line new-cirle new-rectangle)))

(defhelp set-text-alignment
    (use "(set-text-alignment text sym)")
  (info "Set the alignment of #text to #sym, which must be one of '(leading center trailing).")
  (type proc)
  (arity 2)
  (topic (gui drawing))
  (see (new-text set-text-size set-text-style)))

(defhelp set-text-size
    (use "(set-text-size text size)")
  (info "Set the size of #text to float #size.")
  (type proc)
  (arity 2)
  (topic (gui drawing))
  (see (new-text set-text-alignment set-text-style)))

(defhelp set-text-style
    (use "(set-text-style text li")
  (info "Set the style of #text to the specification in list #li, which must contain symbols in '(bold italic monospace symbol tab-width). If a symbol in the list is #tab-width, it must be followed by an integer. #bold sets boldface, #italic makes the style italic, #monospace selects the monospace/typewriter font, and #symbol selects the #symbol font. #tab-width followed by an integer sets the width of tabulator in terms of the number of space characters.")
  (type proc)
  (arity 2)
  (topic (gui drawing))
  (see (new-text set-text-alignment set-text-size)))

(defhelp new-raster-with-pixels
    (use "(new-raster-with-pixels pixel-proc) => int")
  (info "Create a new raster image generated dynamically by the given #pixel-proc. The #pixel-proc takes #x and #y pixel coordinates and the #width and #height of the image in pixels, and returns the color of the pixel #x, #y as a color list of the form '(red green blue [alpha]) where #alpha is optional. Notice that specifying the color of each pixel can be very CPU-intensive for larger images, so optimizations might be necessary.")
  (type proc)
  (arity 1)
  (topic (gui drawing))
  (see (new-image-from-file)))

(defhelp disable-object
    (use "(disable-object obj)")
  (info "Disable the canvas object #obj.")
  (arity 1)
  (type proc)
  (topic (gui canvas-object))
  (see (enable-object hide-object show-object object-disabled? move-object resize-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp enable-object
    (use "(enable-object obj)")
  (info "Enable the canvas object #obj.")
  (arity 1)
  (type proc)
  (topic (gui canvas-object))
  (see (disable-object hide-object show-object object-disabled? move-object resize-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp hide-object
    (use "(hide-object obj)")
  (info "Hide the canvas object #obj.")
  (arity 1)
  (type proc)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object object-disabled? move-object resize-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp show-object
    (use "(show-object obj)")
  (info "Show the canvas object #obj.")
  (arity 1)
  (type proc)
  (topic (gui canvas-object))
  (see (disable-object enable-object hide-object object-disabled? move-object resize-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp object-disabled?
    (use "(object-disabled? obj) => bool")
  (info "Return true if the canvas object #obj is disabled, nil otherwise.")
  (arity 1)
  (type proc)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object move-object resize-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp move-object
    (use "(move-object obj position)")
  (info "Move the canvas object #obj to the given #position list, containing its #x and #y coordinates as floats.")
  (type proc)
  (arity 2)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object object-disabled? resize-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp resize-object
    (use "(resize-object obj width height)")
  (info "Resize canvas object #obj to the given #width and #height as floats.")
  (type proc)
  (arity 2)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object object-disabled? move-object get-object-size get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp get-object-size
    (use "(get-object-size obj) => li")
  (info "Return the size of canvas object #obj as a list containing the width and height as floats.")
  (type proc)
  (arity 1)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object object-disabled? move-object resize-object get-object-min-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp get-object-min-size
    (use "(get-object-min-size obj) => li")
  (info "Return the minimum size of canvas object #obj as a list containing the width and height as floats. The minimum size is computed based on various internal criteria and can only be changed for some special widgets.")
  (type proc)
  (arity 1)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object object-disabled? move-object resize-object get-object-size get-object-position object-visible? refresh-object new-entry new-label)))

(defhelp get-object-position
    (use "(get-object-position obj) => li")
  (info "Return the position of canvas object #obj as a list containing the x and y coordinates as floats.")
  (type proc)
  (arity 1)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object object-disabled? move-object resize-object get-object-size get-object-min-size object-visible? refresh-object new-entry new-label)))

(defhelp refresh-object
    (use "(refresh-object obj)")
  (info "Refresh the canvas object #obj, causing the graphical display to be re-drawn as soon as possible. This may be needed if the object's state has changed.")
  (type proc)
  (arity 1)
  (topic (gui canvas-object))
  (see (disable-object enable-object show-object hide-object object-disabled? move-object resize-object get-object-size get-object-min-size object-visible? get-object-position new-entry new-label)))

(defhelp new-progress-bar
    (use "(new-progress-bar) => int")
  (info "Create a new progress bar whose default minimum is 0.0 and maximum is 1.0.")
  (type proc)
  (arity 0)
  (topic (gui progress-bar))
  (see (set-progress-bar new-infinite-progress-bar get-progress-bar-value)))

(defhelp set-progress-bar
    (use "(set-progress-bar bar value [selector value])")
  (info "Set the value of progress-bar #bar as follows. If a single number is provided, then the current value of the progress-bar is set to this number. If a selector symbol is provided, then if it is 'value, the progress-bar value is set to the following number, if it is 'max or 'min, then the progress-bar maximum or minimum values are set to the respective following number. If it is 'formatter, then the following value must be a procedure that takes the progress-bar ID as argument and returns a string that represents the display of the progress-bar at the given time.")
  (type proc)
  (arity -2)
  (topic (gui progress-bar))
  (see (get-progress-bar-value new-progress-bar new-infinite-progress-bar)))

(defhelp get-progress-bar-value
    (use "(get-progress-bar-value bar) => num")
  (info "Return the current value of progress-bar #bar.")
  (type proc)
  (arity 1)
  (topic (gui progress-bar))
  (see (set-progress-bar new-progress-bar new-infinite-progress-bar)))

(defhelp new-slider
    (use "(new-slider min max proc) => int")
  (info "Create a new slider that allows users to adjust numerical values. The #min and #max arguments must be floats. The procedure #proc takes the current slider float value as argument and is called when the slider changes.")
  (type proc)
  (arity 3)
  (topic (gui slider))
  (see (set-slider-value)))

(defhelp set-slider-value
    (use "(set-slider-value slider fl)")
  (info "Set the value of #slider to float #fl.")
  (type proc)
  (arity 2)
  (topic (gui slider))
  (see (new-slider)))

(defhelp new-icon
    (use "(new-icon resource) => int")
  (info "Create a new icon from #resource, which must be suitable to create an image.")
  (type proc)
  (arity 1)
  (topic (gui icon))
  (see (theme-icon)))

(defhelp forget-gui-object
    (use "(forget-gui-object int)")
  (info "Forget the GUI object #int. This removes any association with the object but does not free internal resources if the object still exists. Internal use only.")
  (type proc)
  (arity 1)
  (topic (gui misc))
  (see (close-window close-gui)))

(defhelp close-gui
    (use "(close-gui)")
  (info "Close the GUI, freeing all resources associated with it. After this function has been called, no further GUI functions can be used.")
  (type proc)
  (arity 0)
  (topic (gui misc))
  (see (forget-gui-object close-window)))

(defhelp get-clipboard-content
    (use "(get-clipboard-content) => str")
  (info "Return the current content of the operating system clipboard as string. This function might raise an error if clipboard access is prohibited by host security settings.")
  (type proc)
  (arity 0)
  (topic (gui misc))
  (see (set-clipboard-content)))

(defhelp set-clipboard-content
    (use "(set-clipboard-content str)")
  (info "Set the operating system clipboard content to string #str. This function might raise an error if clipboard access is prohibited by host security settings.")
  (type proc)
  (arity 1)
  (topic (gui misc))
  (see (get-clipboard-content)))

(defhelp get-device-info
    (use "(get-device-info) => li")
  (info "Return a list with information about the current host device. This returns an association list where 'orientation might be one of '(vertical vertical-upside-down left right unknown), self-explanatory boolean keys 'is-mobile?, 'is-browser, 'has-keyboard?, and 'system-scale with the current scaling factor for graphics as float. The system scale is used to dynamically scale user interface elements to remain legible on hi res displays.")
  (type proc)
  (arity 0)
  (topic (gui misc))
  (see (close-gui)))

(defhelp new-spacer
    (use "(new-spacer) => int")
  (info "Create a new spacer, which adjusts size dynamically by taking up space and displaying nothing. Use this to fill containers e.g. to right align a widget.")
  (type proc)
  (arity 0)
  (topic (gui layout))
  (see (new-hbox-layout new-vbox-layout new-grid-layout new-grid-wrap-layout new-form-layout new-center-layout new-stack-layout new-container)))

(defhelp new-hbox-layout
    (use "(new-hbox-layout) => int")
  (info "Create a new horizontal box layout, which lays out container elements horizontally.")
  (type proc)
  (arity 0)
  (topic (gui layout))
  (see (new-spacer new-vbox-layout new-grid-layout new-grid-wrap-layout new-form-layout new-center-layout new-stack-layout new-container)))

(defhelp new-vbox-layout
    (use "(new-vbox-layout) => int")
  (info "Create a new vertical box layout, which lays out container elements vertically.")
  (type proc)
  (arity 0)
  (topic (gui layout))
  (see (new-spacer new-hbox-layout new-grid-layout new-grid-wrap-layout new-form-layout new-center-layout new-stack-layout new-container)))

(defhelp new-grid-layout
    (use "(new-grid-layout n) => int")
  (info "Create a new grid layout, which arranges elements in #n columns.")
  (type proc)
  (arity 1)
  (topic (gui layout))
  (see (new-spacer new-hbox-layout new-vbox-layout new-grid-wrap-layout new-form-layout new-center-layout new-stack-layout new-container)))

(defhelp new-grid-wrap-layout
    (use "(new-grid-wrap-layout width height) => int")
  (info "Create a new grid wrap layout, which arranges elements such that each element has the given #width and #height, and wraps lines based on the size of the parent container.")
  (type proc)
  (arity 2)
  (topic (gui layout))
  (see (new-spacer new-hbox-layout new-vbox-layout new-grid-layout new-form-layout new-center-layout new-stack-layout new-container)))

(defhelp new-form-layout
    (use "(new-form-layout) => int")
  (info "Create a form layout, which arranges elements in two columns per row, where the columns are aligned.")
  (type proc)
  (arity 0)
  (topic (gui layout))
  (see (new-form append-form new-spacer new-hbox-layout new-vbox-layout new-grid-layout new-grid-wrap-layout new-center-layout new-stack-layout new-container)))

(defhelp new-center-layout
    (use "(new-center-layout) => int")
  (info "Create a new center layout, which centers container elements (possibly overlapping). This may be used for drawing centered on the window, for example.")
  (type proc)
  (arity 0)
  (topic (gui layout))
  (see (new-form append-form new-spacer new-hbox-layout new-vbox-layout new-grid-layout new-grid-wrap-layout new-form-layout new-stack-layout new-container)))

(defhelp new-stack-layout
    (use "(new-stack-layout) => int")
  (info "Create a new stack layout that stacks container elements on top of each other, overlapping. This may be used for drawing, for example.")
  (type proc)
  (arity 0)
  (topic (gui layout))
  (see (new-form append-form new-spacer new-hbox-layout new-vbox-layout new-grid-layout new-grid-wrap-layout new-form-layout new-center-layout new-container)))

(defhelp new-container
    (use "(new-container layout obj ...) => int")
  (info "Create a new container with the given #layout and various canvas objects #obj arranged by the layout.")
  (type proc)
  (arity -2)
  (topic (gui container))
  (see (new-container-without-layout new-border new-vscroll new-hscroll)))

(defhelp new-container-without-layout
    (use "(new-container-without-layout obj ...) => int")
  (info "Create a new container without a layout (overlapping objects) with the given canvas objects #obj.")
  (type proc)
  (arity -1)
  (topic (gui container))
  (see (new-container new-border)))

(defhelp new-border
    (use "(new-border top bottom left right [obj ...]) => int")
  (info "Create a new border layout, which is one of the most useful layouts. Any of #top, #bottom, #left, and #right is put in the respective place (with minimum size) and might also be #nil for no widget. The remaining canvas objects #obj are arranged in the center and take maximum size. This allows you e.g. to put a list on the left side of a window, a panel of buttons on the top, and the main content in another container in the center.")
  (type proc)
  (arity -5)
  (topic (gui container))
  (see (new-container new-container-without-layout new-vscroll new-hscroll)))

(defhelp new-hscroll
    (use "(new-hscroll obj) => int")
  (info "Embed canvas object #obj into a new horizontal scroll container, which allows the user to scroll horizontally if #obj does not fit into the hscroll container horizontally.")
  (type proc)
  (arity 1)
  (topic (gui container))
  (see (new-scroll new-vscroll new-container new-hbox-layout)))

(defhelp new-scroll
    (use "(new-scroll obj) => int")
  (info "Embed canvas object #obj into a new scroll container, which allows the user to scroll both horizontally and vertically if #obj does not fit into the scroll container.")
  (type proc)
  (arity 1)
  (topic (gui container))
  (see (new-vscroll new-hscroll new-container new-hbox-layout)))

(defhelp new-vscroll
    (use "(new-vscroll obj) => int")
  (info "Embed canvas object #obj into a new vertical scroll container, which allows the user to scroll vertically if #obj does not fit into the vscroll container vertically.")
  (type proc)
  (arity 1)
  (topic (gui container))
  (see (new-scroll new-hscroll new-container new-vbox-layout)))

(defhelp get-scroll-offset
    (use "(get-scroll-offset scroll) => li")
  (info "Get the offset of #scroll, which may be a hscroll, vscroll, or scroll, as a position list of (x y) where #x and #y are floats.")
  (type proc)
  (arity 1)
  (topic (gui container))
  (see (set-scroll-offset new-scroll new-hscroll new-vscroll)))

(defhelp set-scroll-offset
    (use "(set-scroll-offset scroll li)")
  (info "Set the #scroll offset to #li, which is a position of the form (x y) where #x and #y are floats. If you don't want to change #x or #y respectively, you need to use #get-scroll-offset first to get the value that you don't want to change, and construct the position from that.")
  (type proc)
  (arity 2)
  (topic (gui container))
  (see (get-scroll-offset new-scroll new-hscroll new-vscroll)))

(defhelp new-tabitem
    (use "(new-tabitem title obj) => int")
  (info "Create a new tab item for use in app-tabs and doc-tabs with a #title and an embedded canvas object #obj shown when the tab item is selected in the tabs.")
  (type proc)
  (arity 2)
  (topic (gui tabs))
  (see (new-tabitem-with-icon new-app-tabs new-doc-tabs)))

(defhelp new-tabitem-with-icon
    (use "(new-tabitem-with-icon title icon obj) => int")
  (info "Create a new tab item for use in app-tabs and doc-tabs with given #title string, #icon resource, and embedded canvas object #obj that shwon when the tab item is selected in the tabs.")
  (type proc)
  (arity 3)
  (topic (gui tabs))
  (see (new-tabitem new-app-tabs new-doc-tabs)))

(defhelp new-app-tabs
    (use "(new-app-tabs tab-item ...) => int")
  (info "Create a new application tabs, which allow users to choose different items within an application.")
  (type proc)
  (arity -1)
  (topic (gui tabs))
  (see (new-doc-tabs new-tabitem new-tabitem-with-icon)))

(defhelp new-doc-tabs
    (use "(new-doc-tabs tab-item ...) => int")
  (info "Create new document tabs, which allow users to choose different items in a window (not the application as a whole like app-tabs).")
  (type proc)
  (arity -1)
  (topic (gui tabs))
  (see (new-app-tabs new-tabitem new-tabitem-with-icon)))

(defhelp theme-icon
    (use "(theme-icon selector) => int")
  (info "Obtain a pre-defined icon from the application's theme based on the symbol #selector, which may be one of '(cancel check-button check-button-checked color-achromatic color-chromatic color-palette computer confirm content-add content-clear content-copy content-cut content-paste content-redo content-remove content-undo delete document-create document-print document download error file-application file-audio file-image file-text file-video file folder-new folder-open folder grid help history home info list login logout mail-attachment mail-compose mail-forward mail-reply-all mail-reply mail-send media-fast-forward media-fast-rewind media-music media-pause media-photo media-play media-record media-replay media-skip-next media-skip-previous media-stop media-video media-expand menu more-horizontal more-vertical move-down move-up navigate-back navigate-next question radio-button radio-button-checked search-replace search settings storage upload view-full-screen view-refresh view-restore visibility-off visibility volume-down volume-mute volume-up warning).")
  (type proc)
  (arity 1)
  (topic (gui theme))
  (see (new-icon new-image-from- new-image-from-resource)))

(defhelp add-canvas-shortcut
    (use "(add-canvas-shortcut canvas shortcut proc)")
  (info "Add the given #shortcut to the given #canvas, calling the handler #proc when it is triggered. #shortcut must be a list consisting of valid keyboard modifier symbols and a valid key symbol. #proc must be a function that takes a shortcut as argument. If multiple non-modifier keys are present, only the last one is taken. However, multiple modifier keys are possible. Possible modifiers are symbols or corresponding strings in '(shift control alt suprt). Possible keys are in '(escape return tab backspace insert delete right left down up page-up page-down home end f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 enter 0 1 2 3 4 5 6 7 8 9 key0 key1 key2 key3 key4 key5 key6 key7 key8 key9 a b c d e f g h i j k l m n o p q r s t u v w x y z space tick comma minus period slash backslash left-bracket right-bracket semicolon equal asterisk plus back-tick) and their string variants.")
  (type proc)
  (arity 3)
  (topic (gui canvas shortcut))
  (see (get-window-canvas remove-canvas-shortcut)))

(defhelp remove-canvas-shortcut
    (use "(remove-canvas-shortcut canvas shortcut)")
  (info "Remove the #shortcut from #canvas, where #shortcut is a list consisting of valid keyboard modifier symbols and a valid key symbol.")
  (type proc)
  (arity 2)
  (topic (gui canvas shortcut))
  (see (add-canvas-shortcut get-window-canvas)))

(defhelp set-canvas-on-typed-key
    (use "(set-canvas-on-typed-key canvas proc)")
  (info "Set the procedure #proc called when a key is typed in #canvas. #proc takes two arguments, the first one is a platform-independent key symbol and the second one is a platform- and keyboard-dependent hardware scancode.")
  (type proc)
  (arity 2)
  (topic (gui canvas))
  (see (set-canvas-on-typed-rune add-canvas-shortcut get-window-canvas)))

(defhelp set-canvas-on-typed-rune
    (use "(set-canvas-on-typed-rune canvas proc)")
  (info "Set the procedure #proc called when a rune is typed in #canvas. #proc takes one argument, a string containing a single Unicode rune.")
  (type proc)
  (arity 2)
  (topic (gui canvas))
  (see (add-canvas-shortcut get-window-canvas set-canvas-on-typed-key)))

(defhelp set-entry-text-wrap
    (use "(set-entry-text-wrap entry selector)")
  (info "Set or remove the text wrapping of #entry, which is only relevant for multiline entries. #selector must be one of '(none break wrap), where 'none indicates no text wrapping, 'break indicates that words are broken without special wrapping algorithm, and 'word means word wrapping.")
  (type proc)
  (arity 2)
  (topic (gui entry))
  (see (new-entry)))

(defhelp new-hsplit
    (use "(new-hsplit obj1 obj2) => int")
  (info "Return a new horizontal divider between canvas object #obj1 and #obj2. The user can adjust the division by drag & drop.")
  (type proc)
  (arity 2)
  (topic (gui split))
  (see (set-split-offset new-vsplit)))

(defhelp new-vsplit
    (use "(new-vsplit obj1 obj2) => int")
  (info "Return a new vertical divider between canvas object #obj1 and #obj2. The user can adjust the division by drag & drop.")
  (type proc)
  (arity 2)
  (topic (gui split))
  (see (set-split-offset new-hplit)))

(defhelp set-split-offset
    (use "(set-split-offset split offset)")
  (info "Set the offset of #split to float #offset between 0.0 and 1.0. #offset indicates the percentage between the objects shown in the split. If #offset is 0.0, then only the second object is shown, if it is 1.0 then only the first object is shown.")
  (type proc)
  (arity 2)
  (topic (gui split))
  (see (new-vsplit new-hsplit)))

(defhelp focus-canvas-object
    (use "(focus-canvas-object canvas object)")
  (info "Set the focus within #canvas to #object. The object must be a focusable canvas object such as an entry or button.")
  (type proc)
  (arity 2)
  (topic (gui canvas canvas-object))
  (see (get-window-canvas get-focused-canvas-object focus-next-canvas-object focus-previous-canvas-object unfocus-canvas-objects)))

(defhelp focus-next-canvas-object
    (use "(focus-next-canvas-object canvas)")
  (info "Focus the next focusable user interface element in #canvas.")
  (type proc)
  (arity 1)
  (topic (gui canvas canvas-object))
  (see (get-window-canvas focus-canvas-object focus-previous-canvas-object unfocus-canvas-objects get-focused-canvas-object)))

(defhelp focus-previous-canvas-object
    (use "(focus-previous-canvas-object canvas)")
  (info "Focus the previous focusable user interface element in #canvas.")
  (type proc)
  (arity 1)
  (topic (gui canvas canvas-object))
  (see (get-window-canvas focus-canvas-object focus-next-canvas-object unfocus-canvas-objects get-focused-canvas-object)))

(defhelp unfocus-canvas-objects
    (use "(unfocus-canvas-objects canvas)")
  (info "Remove the focus on any user interface element in #canvas.")
  (type proc)
  (arity 1)
  (topic (gui canvas canvas-object))
  (see (get-window-canvas focus-canvas-object focus-next-canvas-object focus-previous-canvas-object get-focused-canvas-object)))

(defhelp get-focused-canvas-object
    (use "(get-focused-canvas-object canvas) => int")
  (info "Obtain the canvas object that is currently focused in #canvas, or nil if there is none.")
  (type proc)
  (arity 1)
  (topic (gui canvas canvas-object))
  (see (get-window-canvas focus-canvas-object focus-next-canvas-object focus-previous-canvas-object)))

(defhelp theme-color
    (use "(theme-color selector) => li")
  (info "Obtain a theme color as color list. #selector must be one of '(foreground background button disabled-button disabled disabled-text error focus hover input-background input-border menu-background overlay-background place-holder pressed primary scroll-bar selection separator shadow success warning).")
  (type proc)
  (arity 1)
  (topic (gui theme))
  (see (theme-icon nrgba64 nrgba color->color-64 color-64->color *colors*)))

(defhelp theme-is-dark?
    (use "(theme-is-dark?) => bool")
  (info "Return true if the current GUI theme is dark, nil otherwise.")
  (type proc)
  (arity 1)
  (topic (gui theme))
  (see (theme-color *colors*)))
    
(defhelp color->color64
    (use "(color->color64 li) => li")
  (info "Convert a 32-bit NRGBA color list with values from 0 to 255 (inclusive) as it is used by Z3S5 Lisp's *color* dict to a 64-bit NRGBA64 color list with values from 0 to 65635 (inclusive) as they are used by the GUI.")
  (type proc)
  (arity 1)
  (topic (gui theme))
  (see (theme-color the-color *colors*)))

(defhelp color64->color
    (use "(color64->color li) => li")
  (info "Convert a 64-bit NRGBA64 color list with values from 0 to 65365 (inclusive) to a 32-bit color list with values from 0 to 255 (inclusive) as they are used by Z3S5 Lisp's *colors* dict.")
  (type proc)
  (arity 1)
  (topic (gui theme))
  (see (theme-color the-color *colors*)))

(defhelp create-lorem-ipsum
    (use "(create-lorem-ipsum selector min max) => str")
  (info "Create random Lorem Ipsum fill text based on #selector. If #selector is 'word, then a word with at least #min letters and at most #max letters is created. If #selector is 'sentence, then a sentence with at least #min words and at most #max words is created. If #selector is 'paragraph, then a paragraph with at least #min sentences and at most #max sentences is created.")
  (type proc)
  (arity 3)
  (topic (gui sequences))
  (see (new-zgrid new-text-grid new-entry))) 
  
  
