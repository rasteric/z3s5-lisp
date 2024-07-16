;;; zedit-based Lisp editor

(setq *lisp-editor-info-bg-color*
      (if (theme-is-dark?)
	  (color->color64 (the-color 'firebrick))
	  (color->color64 (the-color 'light-salmon))))

(setq *lisp-editor-info-fg-color*
      (if (theme-is-dark?)
	  (color->color64 (the-color 'white))
	  (color->color64 (the-color 'black))))

(setq *lisp-editor-arg-fg-color*
      (if (theme-is-dark?)
	  (color->color64 (the-color 'spring-green))
	  (color->color64 (the-color 'sea-green))))

(defun _zedit-pad (ed s)
  (fmt (str+ "%-" (fmt "%v"(get-zedit-columns ed)) "s") s))

(defun display-zedit-help-entry (ed entry)
  (delete-zedit-all ed)
  (let ((heading (make-or-get-zedit-style-tag
		  ed
		  (list
		   (list 'bold t)
		   (list 'text-color *lisp-editor-info-fg-color*)
		   (list 'background-color *lisp-editor-info-bg-color*))
		  t)))
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
		      (list (make-or-get-zedit-style-tag
			     ed
			     (list
			      (list 'italic t)
			      (list 'text-color *lisp-editor-arg-fg-color*))
			     nil)))
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

(defclass lisped nil zed iwin frame search choice (terms nil))

(defmethod lisped-add-search-term (this term)
  (setprop this 'terms (remove-duplicates (cons term (prop this 'terms))))
  (set-select-options (prop this 'choice) (prop this 'terms)))

;; Attach the editor to the given a windows with the given canvas. This also sets up
;; the online help system and appropriate event handlers for it.
(defmethod lisped-attach (this win canvas)
  (letrec ((ed (new-zedit 80 40 canvas))
	   (info (new-zedit 80 18 canvas))
	   (search (new-entry))
	   (choice (new-choice 'select '() (lambda (s) (set-entry-text search s))))
	   (search-box (new-border nil nil nil
				   choice search))
	   (vb (new-container (new-vbox-layout) search-box info))
	   (hb (new-container (new-hbox-layout) ed vb)))
    (set-zedit-config info 'draw-caret? nil)
    (set-zedit-config info 'liberal-get-word-at? t)
    (set-zedit-config ed 'show-line-numbers? t)
    (set-zedit-config ed 'get-word-at-left? t)
    (set-zedit-config ed 'liberal-get-word-at? t)
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
       (out (lisped-estimate-sexpr-range this (get-zedit-caret (prop this 'zed))))(nl)
       (set-entry-text search (get-zedit-current-word zedit))
       (focus-zedit zedit)))
    (set-zedit-event-handler
     info
     'select-word
     (lambda (evt zedit)
       (set-entry-text search (zedit-current-selection-text zedit))
       (focus-zedit ed)))
    (setprop this 'choice choice)
    (setprop this 'zed ed)
    (setprop this 'iwin info)
    (setprop this 'frame hb)
    (setprop this 'search search)))

;; Based on the given position list,uses heuristics to find a lisp sexpr
;; start and end as a list of two position lists. This method uses some primitive parsing,
;; not taking into account escape sequences and strings, and assumes that the start of an
;; sexpr is on a separate line with no leading space. This captures top-level
;; s-expressions as long as they are properly indented.
(defmethod lisped-estimate-sexpr-range (this pos)
  (let ((start-pos (_estimate-sexpr-start (prop this 'zed) (list pos t))))
    (list (1st start-pos)
	  (_estimate-sexpr-end (prop this 'zed) start-pos))))

(defun _estimate-sexpr-start (ed pos)
  (if (or (not (2nd pos t))
	  (and (= (2nd (1st pos) 1) 0)
	       (equal? (get-zedit-char-at ed (1st pos)) "(")
	       (not (equal? (get-zedit-char-at ed (1st (get-zedit-prev-pos ed (1st pos))))
			    (get-zedit-config ed 'soft-lf)))))
      pos
      (_estimate-sexpr-start ed (get-zedit-prev-pos ed (1st pos)))))

(defun _estimate-sexpr-end (ed startpos)
  (_estimate-sexpr-end1 ed startpos 0 t))

(defun _estimate-sexpr-end1 (ed pos counter count?)
  (let ((c (get-zedit-char-at ed (1st pos))))
    (cond
      ((not (2nd pos t)) (1st pos))
      ((and (not count?)(not (equal? c "\"")))
       (_estimate-sexpr-end1 ed (get-zedit-next-pos ed (1st pos)) counter count?))
      ((and count? (equal? c "("))
       (_estimate-sexpr-end1 ed (get-zedit-next-pos ed (1st pos))(add1 counter) count?))
      ((and count? (equal? c ")"))
       (if (<= counter 1)
	   (1st pos)
	   (_estimate-sexpr-end1 ed (get-zedit-next-pos ed (1st pos))(sub1 counter) count?)))
      ((equal? c "\"")
       (_estimate-sexpr-end1 ed (get-zedit-next-pos ed (1st pos)) counter (not count?)))
      (t (_estimate-sexpr-end1 ed (get-zedit-next-pos ed (1st pos)) counter count?)))))

;;; SYNTAX COLORING

(defmethod lisped-syntax-color-buffer (this)
  (let ((ed (prop 'zed this)))
    (todo)))
      
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

