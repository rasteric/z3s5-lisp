;;; local init.lisp file
;;; This file is loaded when it is in the same directory as the z3 executable.
;;; Feel free to hack this file as you like!

(defun print-start-banner ()
   (let ((tc (color 'text))
	 (bc (color 'back)))
     (set-color 'text (the-color 'z3s5-sysmsg-text))
     (out "Welcome to ")
     (set-color 'back (the-color 'z3s5-orange))
     (set-color 'text (the-color 'z3s5-blue))
     (out "Z3S5 Lisp")
     (set-color 'back bc)
     (set-color 'text (the-color 'z3s5-sysmsg-text))
     (out " on ")(out (caddr (sys 'version nil)))(out " with ")
     (out (cadr (sys 'version nil)))
     (out " cores!\n")
     (out "The session started on ")(out (datestr (now)))(out " UTC.\n")
     (out "Enter (exit) to close the session. Happy hacking!\n")))

(when *interactive-session* (print-start-banner))
