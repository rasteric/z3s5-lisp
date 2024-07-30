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

(setq *lisp-editor-paren-color* 
 (if (theme-is-dark?)
  (color->color64 (the-color 'orange))
  (color->color64 (the-color 'maroon))))

(setq *lisp-editor-expr-color*
 (if (theme-is-dark?)
  (color->color64 (the-color 'aqua))
  (color->color64 (the-color 'dark-blue))))

(setq *lisp-editor-comment-color*
 (if (theme-is-dark?)
  (color->color64 (the-color 'olive))
  (color->color64 (the-color 'olive-drab))))

(setq *lisp-editor-const-color*
 (if (theme-is-dark?)
  (color->color64 (the-color 'medium-spring-green))
  (color->color64 (the-color 'teal))))

(setq *lisp-editor-defined-symbol-color*
 (if (theme-is-dark?)
  (color->color64 (the-color 'pale-turquoise))
  (color->color64 (the-color 'dark-blue))))

(setq *lisp-editor-paren-style*
 (list
  (list 'text-color *lisp-editor-paren-color*)))

(setq *lisp-editor-comment-style*
 (list
  (list 'text-color *lisp-editor-comment-color*)))

(setq *lisp-editor-const-style*
 (list
  (list 'text-color *lisp-editor-const-color*)))

(setq *lisp-editor-defined-symbol-style*
 (list
  (list 'text-color *lisp-editor-defined-symbol-color*)))

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

(defclass lisped nil zed iwin frame search choice win (terms nil))

(defmethod lisped-add-search-term (this term)
 (setprop this 'terms (remove-duplicates (cons term (prop this 'terms))))
 (set-select-options (prop this 'choice) (prop this 'terms)))

(defun _zed.symli->str (li)
 (apply str+ (map li (lambda (x) (str+ (sym->str x) " ")))))

;; Attach the editor to the given a windows with the given canvas. This also sets up
;; the online help system and appropriate event handlers for it.
(defmethod lisped-attach (this win canvas)
 (letrec ((ed (new-zedit 100 42 canvas))
	  (info (new-zedit 80 18 canvas))
	  (search (new-entry))
	  (choice (new-choice 'select '() (lambda (s) (set-entry-text search s))))
	  (search-box (new-border nil nil nil
		       choice search))
          (topic-choice (new-choice 'select (map (help-topics) sym->str)
                         (lambda (s) (set-zedit-text topic-info
                                      (_zed.symli->str (help-about (str->sym s)))))))
          (topic-form (new-form))
          (topic-info (new-zedit 80 18 canvas))
	  (vb (new-container (new-vbox-layout) search-box info topic-form topic-info))
	  (hb (new-container (new-hbox-layout) ed vb)))
  (append-form topic-form "By topics:" topic-choice)
  (set-zedit-config topic-info 'draw-caret? nil)
  (set-zedit-config topic-info 'liberal-get-word-at? t)
  (set-zedit-config topic-info 'show-line-numbers? nil)
  (set-zedit-config topic-info 'get-word-at-left? t)
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
   'on-change
   (lambda (evt zedit)
    (lisped-syntax-color-buffer this)))
  (set-zedit-event-handler
   ed
   'word-change
   (lambda (evt zedit)
    (set-entry-text search (get-zedit-current-word zedit))
    (focus-zedit zedit)))
  (set-zedit-event-handler
   topic-info
   'word-change
   (lambda (evt zedit)
    (set-entry-text search (get-zedit-current-word zedit))))
  (set-zedit-event-handler
   info
   'select-word
   (lambda (evt zedit)
    (set-entry-text search (zedit-current-selection-text zedit))
    (focus-zedit ed)))
  (setprop this 'choice choice)
  (setprop this 'zed ed)
  (setprop this 'win win)
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

(defun add-lisp-editor (win)
 (let ((canvas (get-window-canvas win))
       (ed (new lisped)))
  (lisped-attach ed win canvas)
  ed))

(defhelp add-lisp-editor
 (use "(add-lisp-editor win)")
 (info "Add a lisp editor to the given window. This includes automatic help and looking up functions.")
 (type proc)
 (arity 1)
 (topic (gui zedit))
 (see (lisped)))

;;; FILE IO

(defmethod lisped-load (this file)
 (let ((in (open file)))
  (with-final (lambda (err result)(close in))
   (let ((s (readall-str in)))
    (cond
     ((str-exists? s (lambda (x) (equal? x 9)))
      (show-confirm
       "Z3S5 Lisp Editor: Convert tabs to spaces?"
       (fmt "The Lisp editor does not support tab characters and has to convert tabs in file \"%v\" to spaces. Do you wish to convert them? If you press No, the file is loaded without modifications but tab characters will not be displayed correctly." file)
       (lambda (ok) (if ok
                     (set-zedit-text (prop this 'zed) (str-replace* s "\t" "  "))
                     (set-zedit-text (prop this 'zed) s)))
       (prop this 'win)))
     (t 
      (set-zedit-text (prop this 'zed) s)))))))

;;; SYNTAX COLORING

(defstruct _zed.state (result nil))

(setq *lisp-editor-wait* (dict))

(defmethod lisped-syntax-color-buffer (this)
 (letrec ((ed (prop this 'zed))
	  (last-line (get-zedit-last-line ed))
	  (last-column (get-zedit-last-column ed last-line)))
  (future 
   (wait-for* *lisp-editor-wait* 'colored 1000)
   (delete *lisp-editor-wait* 'colored)
   (lisped-syntax-color this (list (list 0 0)(list last-line last-column)) t)
   (set *lisp-editor-wait* 'colored t))))

;; Syntax color range, which is expected to indicate the range of an expression
;; but may end too soon when the user is typing. If clear-all? is true, all
;; tags are cleared before the new ones are applied.
(defmethod lisped-syntax-color (this range clear-all?)
 (letrec ((from (1st range))
	  (to (2nd range))
	  (ed (prop this 'zed))
	  (paren-tag
	   (make-or-get-zedit-style-tag
	    ed
	    *lisp-editor-paren-style*
	    nil))
	  (comment-tag
	   (make-or-get-zedit-style-tag
	    ed
	    *lisp-editor-comment-style*
	    nil))
	  (symbol-tag
	   (make-or-get-zedit-style-tag
	    ed
	    *lisp-editor-defined-symbol-style*
	    nil))
	  (const-tag
	   (make-or-get-zedit-style-tag
	    ed
	    *lisp-editor-const-style*
	    nil))
	  (tokens (_zed.syntax-color this from to (list from t) (make _zed.state nil))))
  (if clear-all?
   (clear-zedit-tags ed)
   (clear-zedit-range ed range))
  (foreach
   tokens
   (lambda (token)
    (case (1st token)
     ((symbol) (let ((sym (str->sym (get-zedit-text-range ed (list (2nd token)(3rd token))))))
		(when (_bound? sym)
		 (add-zedit-tags ed (list (2nd token)(3rd token))(clone-zedit-tag ed symbol-tag)))))
     ((comment)
      (add-zedit-tags ed (list (2nd token)(3rd token)) (clone-zedit-tag ed comment-tag)))
     ((string)
      (add-zedit-tags ed (list (2nd token) (3rd token)) (clone-zedit-tag ed const-tag)))
     ((rparen lparen)
      (add-zedit-tags ed (list (2nd token) (3rd token)) (clone-zedit-tag ed paren-tag))))))))

(defun _zed.syntax-color (this from to pos state)
 (let ((p (1st pos)))
  (cond
   ((and (2nd pos)
     (or (< (1st p)(1st to))
      (and (= (1st p)(1st to))
       (<= (2nd p)(2nd to)))))
    (let ((token (_zed.next-token (prop this 'zed) p)))
     (cond
      (token
       (_zed.state-result! state (cons token (_zed.state-result state)))
       (_zed.syntax-color this from to
	(get-zedit-next-pos (prop this 'zed)(3rd token)) state))
      (t (_zed.syntax-color this from to (get-zedit-next-pos (prop this 'zed) p) state)))))
   (t
    (_zed.state-result state)))))

;; Get the next token in zedit at given pos.
(defun _zed.next-token (ed charpos)
 (let ((c (get-zedit-char-at ed charpos)))
  (case c
                                        ;   ((";")(_zed.parse-comment ed charpos (get-zedit-next-pos ed charpos)))
   (("\"")(_zed.parse-string ed charpos (get-zedit-next-pos ed charpos)))
   ((")") (list 'rparen charpos charpos))
   (("(") (list 'lparen charpos charpos))
   (t (cond
       ((_zed.is-symbol-char? c)
	(_zed.parse-symbol ed charpos (get-zedit-next-pos ed charpos)))
       (t nil))))))

(defun _zed.is-symbol-char? (c)
 (and (not (or (unicode.is-space? c)
	    (unicode.is-control? c)
	    (equal? c ";")
	    (equal? c "\"")
	    (equal? c "(")
	    (equal? c ")")))
  (unicode.is-graphic? c)))

(defun _zed.parse-symbol (ed start pos)
 (cond
  ((not (2nd pos))
   (list 'symbol start (1st pos)))
  (t (let ((c (get-zedit-char-at ed (1st pos))))
      (cond
       ((_zed.is-symbol-char? c)
	(_zed.parse-symbol ed start (get-zedit-next-pos ed (1st pos))))
       (t (list 'symbol start (1st (get-zedit-prev-pos ed (1st pos))))))))))

(defun _zed.parse-string (ed start pos)
 (cond
  ((not (2nd pos))
   (list 'string start (1st pos)))
  (t (let ((c (get-zedit-char-at ed (1st pos))))
      (cond
       ((equal? c (get-zedit-config ed 'hard-lf))
	(list 'string start (1st pos)))
       ((equal? c "\\") (_zed.parse-string ed start
			 (get-zedit-next-pos
			  ed (1st (get-zedit-next-pos ed (1st pos))))))
       ((equal? c "\"") (list 'string start (1st pos)))
       (t (_zed.parse-string ed start (get-zedit-next-pos ed (1st pos)))))))))

(defun _zed.parse-comment (ed start pos)
 (let ((endline (fine-zedit-paragraph-end ed (1st pos))))
  (list 'comment start (list endline (get-zedit-last-column ed endline)))))

;;; GENERAL

(defun edit-lisp-file (file)
 (letrec ((win (new-window (fmt "Editing \"%v\"" (file-name file))))
          (ed (add-lisp-editor win)))
   (set-window-content win (prop ed 'frame))
  (focus-zedit (prop ed 'zed))
  (show-window win)
  (lisped-load ed file)))

;;; TESTING

(defun test-editor ()
 (edit-lisp-file "editor.lisp"))

