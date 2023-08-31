;;;; This example demonstrates how to create a window and button in the low-level GUI API.
;;;; Note: Due to limitations of Fyne, an application cannot be run twice!

(defun demo1 ()
  (letrec ((win (new-window "Test Window"))
           (button (new-button "Hello world!" (lambda () (close-ui)))))
    (set-window-content win button)
    (show-window win)))

(out "Use (demo1) to run some GUI demo.\n")

