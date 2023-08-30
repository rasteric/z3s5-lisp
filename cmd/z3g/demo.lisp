;;;; This example demonstrates how to create a window and button in the low-level GUI API.
;;;; Note: Due to limitations of Fyne, an application cannot be run twice!

(defun demo1 ()
  (letrec ((a (gui.new-app "com.z3s5.demo1"))
	   (w (gui.new-window a "Test Window"))
	   (b (gui.new-button "Close" (lambda () (gui.window-close w)(exit)))))
    (gui.window-set-content w b)
    (gui.window-set-master w)
    (gui.window-resize w (gui.new-size 300 200))
    (gui.window-show-and-run w)))

(defun demo2 ()
  (letrec ((a (gui.new-app "com.z3s5.demo2"))
	   (w (gui.new-window a "Test Window"))
	   (b (gui.new-button "Close" (lambda () (gui.window-close w)(exit))))
	   (l (gui.new-label "Hello world!"))
	   (c (gui.new-container (gui.new-vbox-layout) l b)))
    (gui.window-set-content w c)
    (gui.window-set-master w)
    (gui.window-show-and-run w)))

(out "Use (demo1) ... (demo2) to run some GUI demos.\n")

