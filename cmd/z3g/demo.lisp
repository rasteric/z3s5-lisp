;;;; This example demonstrates how to create a window and button in the low-level GUI API.
;;;; Note: Due to limitations of Fyne, an application cannot be run twice!

(defun demo1 ()
  (letrec ((win (new-window "Demo 1"))
           (button (new-button "Hello world!" (lambda () (close-window win)))))
    (set-window-content win button)
    (set-window-on-close-callback 
      win (lambda () (out (fmt "window %v was closed\n" win))))
    (show-window win)))

(defun demo2 ()
  (letrec ((win (new-window "Demo 2"))
           (entry (new-entry)))
    (set-window-content win entry)
    (set-window-on-close-callback 
      win (lambda () (out (fmt "window %v was closed\n" win))))
    (set-entry-on-change-callback entry (lambda (s) (out s)(out "\n")))
    (show-window win)))

(defun demo3 ()
  (letrec ((win (new-window "Demo 3: Radio-Group"))
           (select (new-choice 'radio-group '("Option 1" "Option 2" "Option 3" "Option 4" "Option 5") (lambda (s) (out s)(out "\n")))))
    (set-window-content win select)
    (show-window win)))

(defun demo4 ()
  (letrec ((win (new-window "Demo 4: Forms"))
           (form (new-form)))
    (append-form form "Name" (new-entry))
    (append-form form "Address" (new-entry))
    (append-form form "Phone" (new-entry))
    (append-form form "Email" (new-entry))
    (append-form form "More" (new-hyperlink "Click here for more info" "https://z3s5.com"))
    (set-window-content win form)
    (show-window win)))

(defun demo5 ()
  (letrec ((win (new-window "Demo 5"))
           (button (new-button-with-icon "Press me!" 
              (theme-icon 'view-restore)
              (lambda () (close-window win)))))
    (set-window-content win button)
    (show-window win)))

(out "Use (demo1) ... (demo5) to run GUI demos.\n")

