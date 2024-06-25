;;; zedit-based Lisp editor

(setq *lisp-editor-info-bg-color*
      (if (theme-is-dark?)
	  (color->color64 (the-color 'dark-blue))
	  (color->color64 (the-color 'light-blue))))

(setq *lisp-editor-arg-fg-color*
      (if (theme-is-dark?)
	  (color->color64 (the-color 'spring-green))
	  (color->color64 (the-color 'sea-green))))

(defun _zedit-pad (ed s)
  (fmt (str+ "%-" (fmt "%v"(get-zedit-columns ed)) "s") s))

(defun display-zedit-help-entry (ed entry)
  (delete-zedit-all ed)
  (let ((heading (make-or-get-zedit-color-tag
		  ed
		  *lisp-editor-info-bg-color*
		  t t)))
    (print-zedit ed
		 (fmt "%v with %v in %v\n" 
		      (_help-type-to-str (assoc1 'type entry))
		      (_arity->info (assoc1 'arity entry))
		      (assoc1 'topic entry))
		 nil)
    (print-zedit ed "\n" nil)
    (print-zedit ed (_zedit-pad ed (fmt "%v" (assoc1 'use entry)))(list heading))
    (print-zedit ed "\n\n" nil)
    (_display-zedit-info ed (assoc1 'info entry))
    (print-zedit ed "\n\n" nil)
    (print-zedit ed (fmt "See also: %v.\n" (_help-see-to-str (assoc1 'see entry))) nil)))

(defun _display-zedit-info (ed s)
  (mapcar
   (_out-help-segment s "#" " .,:()[]{}")
   (lambda (x)
     (if (1st x nil)
	 (print-zedit ed (2nd x)
		      (list (make-or-get-zedit-color-tag
			     ed
			     *lisp-editor-arg-fg-color*
			     nil nil)))
	 (print-zedit ed (2nd x) nil)))))

(defun _arity->info (x)
  (cond
    ((= x 0)
     "no argument")
    ((= x 1)
     "1 argument")
    ((> x 1)
     (fmt "%v arguments" x))
    (t
     (fmt "%v or more arguments" (if (equal? x -1)
				     0
				     (sub1 (* x -1)))))))

(defclass lisped nil zed iwin frame search prev next (search-terms nil) (search-idx 0))

(defmethod lisped-add-search-term (this term)
  (setprop this 'search-terms (cons term (prop this 'search-terms)))
  (setprop this 'search-idx (len (prop this 'search-terms))))

(defmethod lisped-prev-search (this)
  (setprop this 'search-idx (max (sub1 (prop this 'search-idx)) 0))
  (lisped-current-search this))

(defmethod lisped-next-search (this)
  (setprop this 'search-idx (min (sub1 (len (prop this 'search-terms)))
				 (add1 (prop this 'search-idx))))
  (lisped-current-search this))

(defmethod lisped-current-search (this)
  (nthdef (prop this 'search-terms)(prop this 'search-idx) nil))
   
(defmethod lisped-attach (this win canvas)
  (letrec ((ed (new-zedit 80 40 canvas))
	   (info (new-zedit 80 38 canvas))
	   (search (new-entry))
	   (back (new-button-with-icon "" 
					 (theme-icon 'navigate-back)
					 (lambda () (let ((term (lisped-prev-search this)))
						      (when term (set-entry-text search term))))))
	   (next (new-button-with-icon "" (theme-icon 'navigate-next)
				       	 (lambda () (let ((term (lisped-next-search this)))
						      (when term (set-entry-text search term))))))
	   (search-box (new-border nil nil nil
				   (new-container
				    (new-hbox-layout) back next) search))
	   (vb (new-container (new-vbox-layout) search-box info))
	   (hb (new-container (new-hbox-layout) ed vb)))
    (set-zedit-config info 'draw-caret? nil)
    (set-entry-on-change-callback
     search
     (lambda (s)
       (let ((sym (str->sym s)))
	 (when sym
	   (let ((entry (help-entry sym)))
	     (when entry
	       (lisped-add-search-term this s)
	       (display-zedit-help-entry info entry)))))))
    (set-zedit-event-handler
	ed
     'word-change
     (lambda (evt zedit)
       (set-entry-text search (get-zedit-current-word zedit))
       (focus-zedit zedit)))
    (setprop this 'zed ed)
    (setprop this 'iwin info)
    (setprop this 'frame hb)
    (setprop this 'search search)))

(defun add-lisp-editor (win)
  (let ((canvas (get-window-canvas win))
	(ed (new lisped)))
    (lisped-attach ed win canvas)
    ed))

(defun test-editor ()
  (letrec ((win (new-window "Lisp Editor"))
	   (ed (add-lisp-editor win)))
    (set-window-content win (prop ed 'frame))
    (focus-zedit (prop ed 'zed))
    (show-window win)))

