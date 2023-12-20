;;;; Z3S5 Lisp editor

(setq zed.*blink-cursor-on-interval* 800)
(declare-unprotected 'zed.*blink-cursor-on-interval*)
(setq zed.*blink-cursor-off-interval* 200)
(declare-unprotected 'zed.*blink-cursor-off-interval*)
(setq zed.*paren-style* nil)
(declare-unprotected 'zed.*paren-style*)
(setq zed.*comment-style* nil)
(declare-unprotected 'zed.*comment-style*)
(setq zed.*symbol-style* nil)
(declare-unprotected 'zed.*symbol-style*)
(setq zed.*literal-style* nil)
(declare-unprotected 'zed.*literal-style*)
(setq zed.*functional-style* nil)
(declare-unprotected 'zed.*functional-style*)
(setq zed.*error-style* nil)
(declare-unprotected 'zed.*error-style*)
(setq zed.*unbound-symbol-style* nil)
(declare-unprotected 'zed.*unbound-symbol-style*)
(setq zed.*special-form-style* nil)
(declare-unprotected 'zed.*special-form-style*)
(setq zed.*macro-style* nil)
(declare-unprotected 'zed.*macro-style*)
(setq zed.*highlight-paren-style* nil)
(declare-unprotected 'zed.*highlight-paren-style*)
(setq zed.*special-forms* (dict `(cond ,t setq ,t quote ,t progn ,t)))
(setq zed.*valid-props* '(soft-wrap horizontal-scroll auto-columns))
(setq zed.*default-soft-columns* 80)
(setq zed.*skip-expr-search-on-empty-line* true) ; don't search for end of Lisp expr beyond an empty line
(setq zed.*soft-lf* "\r")
(setq zed.*lf* " ")

(defun zed.blink-cursor (ed)
  (future (letrec ((blink (lambda ()
			    (zed.lock ed)
       			    (zed.set-cursor-state ed (not (zed.cursor-state ed)))
       			    (zed.draw-cursor ed (zed.cursor-state ed))
			    (zed.unlock ed)
       			    (if (zed.cursor-state ed)
       				(sleep zed.*blink-cursor-on-interval*)
       				(sleep zed.*blink-cursor-off-interval*))
       			    (blink))))
       	    (blink))))

(defun zed.new (&rest args)
  (cond
    ((theme-is-dark?)
     (setq zed.*comment-style* (list (color->color64 (lighten (the-color 'brown)))(theme-color 'background)))
     (setq zed.*literal-style* (list (color->color64 (the-color 'pale-green))(theme-color 'background)))
     (setq zed.*symbol-style* (list (color->color64 (lighten (the-color 'sea-green)))(theme-color 'background)))
     (setq zed.*functional-style* (list (color->color64  (the-color 'powder-blue))(theme-color 'background)))
     (setq zed.*special-form-style* (list (color->color64 (the-color 'violet))(theme-color 'background)))
     (setq zed.*error-style* (list (color->color64 (the-color 'crimson))(theme-color 'background)))
     (setq zed.*macro-style* (list (color->color64 (the-color 'light-sky-blue))(theme-color 'background)))
     (setq zed.*unbound-symbol-style* (list (theme-color 'foreground)(theme-color 'background)))
     (setq zed.*highlight-paren-style* (list (color->color64 (lighten (the-color 'dark-gray))) (color->color64 (the-color 'light-salmon))))
     (setq zed.*paren-style* (list (color->color64 (lighten (the-color 'dark-gray)))(theme-color 'background))))
    (t
     (setq zed.*comment-style* (list (color->color64 (the-color 'brown))(theme-color 'background)))
     (setq zed.*literal-style* (list (color->color64 (the-color 'green))(theme-color 'background)))
     (setq zed.*symbol-style* (list (color->color64 (the-color 'sea-green))(theme-color 'background)))
     (setq zed.*error-style* (list (color->color64 (the-color 'red))(theme-color 'background)))
     (setq zed.*special-form-style* (list (color->color64 (the-color 'indigo))(theme-color 'background)))
     (setq zed.*functional-style* (list (color->color64 (the-color 'royal-blue))(theme-color 'background)))
     (setq zed.*macro-style* (list (color->color64 (the-color 'dark-blue))(theme-color 'background)))
     (setq zed.*unbound-symbol-style* (list (theme-color 'foreground)(theme-color 'background)))
     (setq zed.*highlight-paren-style* (list (color->color64 (the-color 'dim-gray)) (color->color64 (the-color 'light-blue))))
     (setq zed.*paren-style* (list (color->color64 (the-color 'dim-gray))(theme-color 'background)))))
  (letrec ((grid (new-text-grid))
	   (props (zed._validate-props (1st args nil)))
	   (columns (2nd args zed.*default-soft-columns*))
	   (scroll (if (member 'horizontal-scroll props)(new-scroll grid)(new-vscroll grid)))
	   (ed (array 'zed.editor grid 0 0 true true (make-mutex) scroll props columns nil nil
		      (dict) nil)))
    (zed.set-text ed "")
    (zed.add-painter ed 'selection (lambda (ed) (zed.paint-selection ed)))
    (zed.add-painter ed 'parens (lambda (ed) (zed.maybe-highlight-parens ed)))
    (zed.draw-cursor ed t)
   ; (zed.blink-cursor ed)
    ed))

;;; ACCESSORS

;; 0 'zed-editor
(defun zed.is-editor? (ed)
  (and (array? ed)
       (> (array-len ed) 0)
       (equal? (array-ref ed 0) 'zed.editor)))

;; 1 the text grid
(defun zed.grid (ed)
  (array-ref ed 1))

;; 2 the cursor row
(defun zed.cursor-row (ed)
  (array-ref ed 2))

(defun zed.set-cursor-row (ed row)
  (array-set ed 2 row))

;; 3 the cursor column
(defun zed.set-cursor-column (ed col)
  (array-set ed 3 col))

(defun zed.cursor-column (ed)
  (array-ref ed 3))

;; 4 bool whether a cursor should be drawn
(defun zed.draw-cursor? (ed)
  (array-ref ed 4))

(defun zed.set-draw-cursor (ed on?)
  (array-set ed 4 on?))

;; 5 bool state of cursor (periodically changes when blinking)
(defun zed.cursor-state (ed)
  (array-ref ed 5))

(defun zed.set-cursor-state (ed on?)
  (array-set ed 5 on?))

;; 6 a mutex for synchronization, since callbacks might from in any goroutine
(defun zed.mutex (ed)
  (array-ref ed 6))

(defun zed.lock (ed)
  (mutex-lock (array-ref ed 6)))

(defun zed.unlock (ed)
  (mutex-unlock (array-ref ed 6)))

;; 7 scroll, which embeds the text-grid for the editor
(defun zed.scroll (ed)
  (array-ref ed 7))

;; 8 properties of editor, a list of selectors for determining the visual appearance
(defun zed.props (ed)
  (array-ref ed 8))

(defun zed.has? (ed flag)
  (member flag (zed.props ed)))

(defun zed._validate-props (li)
  (foreach li
	   (lambda (x)
	     (unless (member x zed.*valid-props*)
	       (error (fmt "zed.new: invalid property '%v, it must be one of '%v" x zed.*valid-props*)))))
  li)

;; 9 columns for soft-wrapping, uses a sanity check <=4 because zed.max-displayed-columns
;; might under certain conditions not return the correct size
(defun zed.soft-columns (ed)
  (if (zed.has? ed 'auto-columns)
      (if (<= (zed.max-displayed-columns ed) 4)(array-ref ed 9)(zed.max-displayed-columns ed))
      (array-ref ed 9)))

(defun zed.set-soft-columns (ed n)
  (array-set ed 9 n))

;; 10 current selection, nil if nothing is selected
;; N.B. a selection might not have an end
(defun zed.selection (ed)
  (array-ref ed 10))

(defun zed.set-selection (ed selection)
  (array-set ed 10 selection))

(defun zed.remove-selection (ed)
  (array-set ed 10 nil))

;; 11 syntax coloring procs, called when editor changes
;; this is an a-list of (key painter) pairs, where order matters
(defun zed.painters (ed)
  (array-ref ed 11))

(defun zed.set-painters (ed li)
  (array-set ed 11 li))

(defun zed.add-painter (ed key painter)
  (array-set ed 11 (cons (list key painter) (array-ref ed 11))))

(defun zed.remove-painter (ed key)
  (array-set ed 11 (filter (array-ref ed 11) (lambda (elem) (not (equal? (1st elem nil) key))))))

(defun zed.has-painter? (ed key)
  (assoc key (array-ref ed 11)))

;; 12 user-dict, used for storing data
;; and restoring it later
(defun zed.user-dict (ed) 
 (array-ref ed 12))

;; 13 help callback, called when the cursor moves onto a symbol
(defun zed.help-cb (ed)
  (array-ref ed 13))

(defun zed.set-help-cb (ed cb)
  (array-set ed 13 cb))

;;; GENERAL EDITING

(defun zed.draw-cursor (ed on?)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (style (2nd (get-text-grid-cell (zed.grid ed) row col)
		       (list (theme-color 'foreground) (theme-color 'input-background))))
	   (fgcolor (theme-color 'foreground)))
    (set-text-grid-style
     (zed.grid ed)
     row
     col
     (cond
       (on?
	(set (zed.user-dict ed) (fmt "%v,%v" row col) style)
	(list (zed.invert-color fgcolor) fgcolor))
       (t
	(get (zed.user-dict ed)(fmt "%v,%v" row col)
	     (list (theme-color 'foreground) (theme-color 'background))))))
    (refresh-object (zed.grid ed)))) 

(defun zed.invert-color (li)
  (let ((r (1st li))
	(g (2nd li))
	(b (3rd li))
	(a (4th li 65535)))
    (list (- 65535 r) (- 65535 g) (- 65535 b) a)))

(defun zed.set-text (ed s)
  (set-text-grid-text
   (zed.grid ed)
   (cond
     ((zed.has? ed 'soft-wrap)
      (zed.soft-wrap-str (str-replace* s "\r\n" "\n")(zed.soft-columns ed)))
     (t
       (str-replace* (str-replace* s "\r\n" "\n") "\n" " \n"))))
  (when (= (count-text-grid-row-columns (zed.grid ed) (zed.last-row ed)) 0)
    (set-text-grid-row (zed.grid ed) (zed.last-row ed) (list (array `(,zed.*soft-lf* nil)) nil)))) 

(defun zed.get-text (ed)
  (let ((result ""))
    (dotimes (n (count-text-grid-rows (zed.grid ed)))
      (let ((s (get-text-grid-row-text (zed.grid ed) n)))
	(case (slice s (sub1 (len s)) (len s))
	  ((zed.*soft-lf*) (setq result (str+ result (slice s 0 (max 0 (sub1 (len s)))))))
	  (("\n") (setq result (str+ result s)))
	  ((zed.*lf*) (setq result (str+ result (slice s 0 (max 0 (sub1 (len s)))) "\n")))
	  (t (out "WARN: zed.get-text - line in editor.lisp does not end in SPC, CR, or LN!\n")
	     (setq result (str+ result s))))))
    result))

(defun zed.style-fg-color (s)
  (1st s '(0 0 0 255)))

(defun zed.style-bg-color (s)
  (2nd s '(255 255 255 255)))

;; (defun zed.handler-call (ed proc)
;;   (with-final
;;       (lambda (err v)
;; 	(zed.paint ed)
;; 	(zed.draw-cursor ed true)
;; 	(when err (*error-printer* err)))
;;     (zed.draw-cursor ed nil)
;;     (proc)))

(defun zed.handler-call (ed proc)
  (zed.draw-cursor ed nil)
  (proc)
  (zed.paint ed)
  (zed.draw-cursor ed t))

;; handle non-alphanumeric keys such as tab,return,backspace
(defun zed.key-handler (ed key code)
  (zed.handler-call ed
   (lambda ()
     (case key
       ((left) (zed.cursor-left ed))
       ((right) (zed.cursor-right ed))
       ((up) (zed.cursor-up ed))
       ((down) (zed.cursor-down ed))
       ((backspace) (zed.backspace ed))
       ((delete) (zed.delete1 ed))
       ((return) (zed.return ed))
       ((home) (zed.cursor-home ed))
       ((end) (zed.cursor-end ed))
       ((page-down) (zed.cursor-half-page-down ed))
       ((page-up) (zed.cursor-half-page-up ed))))))

;; set editor default keyboard shortcuts in the canvas
(defun zed.install-default-shortcuts (ed canvas)
  (add-canvas-shortcut
   canvas '(ctrl e)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-jump-to-line-end ed)))))
  (add-canvas-shortcut
   canvas '(ctrl q)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-jump-to-line-start ed)))))
  (add-canvas-shortcut
   canvas '(ctrl p)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-up ed)))))
  (add-canvas-shortcut
   canvas '(ctrl n)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-down ed)))))
  (add-canvas-shortcut
   canvas '(alt p)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-half-page-up ed)))))
  (add-canvas-shortcut
   canvas '(alt n)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-half-page-down ed)))))
  (add-canvas-shortcut
   canvas '(alt left)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-to-previous-half-word ed)))))
  (add-canvas-shortcut
   canvas '(ctrl k)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.delete-to-right ed)))))
    (add-canvas-shortcut
   canvas '(alt b)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-to-previous-half-word ed)))))
  (add-canvas-shortcut
   canvas '(alt right)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-to-next-half-word ed)
	(zed.cursor-right-on-punctuation ed)))))
   (add-canvas-shortcut
   canvas '(alt f)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-to-next-half-word ed)
	(zed.cursor-right-on-punctuation ed)))))
   (add-canvas-shortcut
    canvas '(alt 0)
    (lambda ()
      (zed.handler-call
       ed
       (lambda ()
	 (error "This is supposed to fail")))))
  (add-canvas-shortcut
   canvas '(ctrl f)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-right ed)))))
  (add-canvas-shortcut
   canvas '(ctrl l)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(out (zed.find-lisp-expression-at ed (zed.cursor-row ed)(zed.cursor-column ed)))
	(out "\n")))))
  (add-canvas-shortcut
   canvas '(ctrl m)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.mark-selection-at-cursor ed)))))
  (add-canvas-shortcut
   canvas '(ctrl b)
   (lambda ()
     (zed.handler-call
      ed
      (lambda ()
	(zed.cursor-left ed))))))

;; handle alphanumeric keys
(defun zed.rune-handler (ed rune)
  (zed.draw-cursor ed nil)
  (let ((pos (wrap-insert-text-grid (zed.grid ed) (array (list rune nil))
				    (zed.cursor-row ed)
				    (zed.cursor-column ed)
				    (zed.soft-columns ed)
				    true zed.*lf* zed.*soft-lf*)))
    (zed.set-cursor-row ed (1st pos (zed.cursor-row ed)))
    (zed.set-cursor-column ed (2nd pos (zed.cursor-column ed))))
  (zed.cursor-right ed)
  (zed.scroll-down-to-cursor ed)
  (zed.draw-cursor ed true))

(defun zed.backspace (ed)
  (letrec ((to-row (zed.cursor-row ed))
	   (to-col (zed.cursor-column ed))
	   (pos (zed.pos-dec ed to-row to-col))
	   (from-row (1st pos 0))
	   (from-col (2nd pos 0)))
    (unless (and (= to-row 0) (= to-col 0))
      (letrec ((new-pos (wrap-delete-text-grid
			 (zed.grid ed)
			 (list from-row from-col to-row to-col)
			 (zed.soft-columns ed)
			 t
			 zed.*lf*
			 zed.*soft-lf*
			 to-row to-col)))
	(zed.set-cursor-row ed (1st new-pos))
	(zed.set-cursor-column ed (2nd new-pos))))))

;; delete the cell under the cursor without moving the cursor (delete key behavior)
(defun zed.delete1 (ed)
  (let ((from-row (zed.cursor-row ed))
	(from-col (zed.cursor-column ed)))
    (unless (zed.last-pos? ed from-row from-col)
      (letrec ((pos (zed.pos-inc ed from-row from-col))
	       (to-row (1st pos))
	       (to-col (2nd pos)))
        (wrap-delete-text-grid
	 (zed.grid ed)
	 (list from-row from-col to-row to-col)
	 (zed.soft-columns ed)
	 t
	 zed.*lf*
	 zed.*soft-lf*
	 to-row to-col)))))

;; delete everything from the cell under the cursor to the end of the line
(defun zed.delete-to-right (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (endcol (zed.last-column ed row)))
    (cond
      ((= col endcol)
       (wrap-delete-text-grid
	(zed.grid ed)
	(list row col row (add1 endcol))
	(zed.soft-columns ed)
	t
	zed.*lf*
	zed.*soft-lf*
	row col))
      (t 
       (wrap-delete-text-grid
	(zed.grid ed)
	(list row col row endcol)
	(zed.soft-columns ed)
	t
	zed.*lf*
	zed.*soft-lf*
	row col)))))
    

;; return creates a new line, copying the rest of the current line to it (return key behavior)
;; edge case: If the cursor is at 0,0 then a new line is created above.
(defun zed.return (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed)))
    (cond
      ((and (= row 0)(= col 0))
       (insert-text-grid-row (zed.grid ed) 0)
       (set-text-grid-row (zed.grid ed) 0 (list (array `(,zed.*lf* nil)) nil))
       (zed.cursor-down ed))
      (t
       (insert-text-grid-row (zed.grid ed) (add1 row))
       (cond
	 ((= col (zed.last-column ed row))
	  (set-text-grid-row (zed.grid ed) (add1 row) (list (array `(,zed.*lf* nil)) nil)))
	 (t
	  (letrec ((row-data (get-text-grid-row (zed.grid ed) row))
		   (row-cells (1st row-data))
		   (row-style (2nd row-data))
		   (row-slice (array-slice row-cells col (len row-cells))))
	    ;; copy from col to last cell (inclusive), which is zed.*lf*, into new line
	    (set-text-grid-row (zed.grid ed) (add1 row) (list row-slice row-style))
	    ;; leave from 0 to col (exclusive) the original line
	    (set-text-grid-row (zed.grid ed) row
			       (list (array-append (array-slice row-cells 0 col) (list zed.*lf* nil)) row-style)))))
       (zed.set-cursor-column ed 0)
       (zed.set-cursor-row ed (add1 row))
       (zed.scroll-left-to-line-start ed)
       (zed.scroll-down-to-cursor ed)
       (when (= (add1 row) (zed.last-row ed)) (zed.cursor-end ed))))))
  

;; the maximum number of fully displayed lines (there may be additional partial lines visible, though)
(defun zed.max-displayed-lines (ed)
  (letrec ((h (2nd (get-text-grid-cell-size (zed.grid ed))))
	   (pixel-height (2nd (get-object-size (zed.scroll ed)))))
    (int (/ pixel-height h))))

;; the maximum number of fully displayed columns (there may be additional partial column glyphs visible, though)
(defun zed.max-displayed-columns (ed)
  (letrec ((w (1st (get-text-grid-cell-size (zed.grid ed))))
	   (pixel-width (1st (get-object-size (zed.scroll ed)))))
    (int (/ pixel-width w))))

(defun zed.first-displayed-line (ed)
  (letrec ((offset (get-scroll-offset (zed.scroll ed)))
	   (hoffset (2nd offset))
	   (delta-h (2nd (get-text-grid-cell-size (zed.grid ed)))))
    (fl.ceil (/ hoffset delta-h))))

(defun zed.first-displayed-column (ed)
  (letrec ((offset (get-scroll-offset (zed.scroll ed)))
	   (woffset (1st offset))
	   (delta-w (1st (get-text-grid-cell-size (zed.grid ed)))))
    (int (/ woffset delta-w))))
     
(defun zed.set-display-size (ed columns lines)
  (letrec ((size (get-text-grid-cell-size (zed.grid ed)))
	   (h (2nd size))
	   (w (1st size)))
    (resize-object (zed.scroll ed) (* w columns)  (* h lines))))

(defun zed.install-key-handler (ed canvas)
  (set-canvas-on-typed-key
   canvas (lambda (key code)
	    (zed.lock ed)
	    (zed.key-handler ed key code)
	    (zed.unlock ed))))

(defun zed.install-rune-handler (ed canvas)
  (set-canvas-on-typed-rune
   canvas (lambda (rune)
	    (zed.lock ed)
	    (zed.rune-handler ed rune)
	    (zed.unlock ed))))
 
(defun zed.cursor-left (ed)
   (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (rowMinus (sub1 row))
	   (colMinus (sub1 col)))
    (cond
      ((and (= row 0) (= col 0)) (void))
      ((> col 0)
       (when (< col (zed.first-displayed-column ed)) (zed.scroll-left ed))
       (zed.set-cursor-column ed colMinus))
      (t
       (zed.set-cursor-column ed (zed.last-column ed rowMinus))
       (zed.scroll-right-to-cursor ed)
       (zed.set-cursor-row ed rowMinus)))))

(defun zed.cursor-right (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (rowPlus (add1 row))
	   (colPlus (add1 col)))
    (cond
      ((zed.last-pos? ed row col)
       (void))
      ((<= colPlus (zed.last-column ed row))
       (when (= (add1 col) (zed.last-displayed-column ed)) (zed.scroll-right ed))
       (zed.set-cursor-column ed colPlus))
      ((<= rowPlus (zed.last-row ed))
	 (zed.set-cursor-column ed 0)
	 (zed.scroll-left-to-line-start ed)
	 (zed.set-cursor-row ed rowPlus)))))

(defun zed.last-column (ed row)
  (sub1 (count-text-grid-row-columns (zed.grid ed) row)))

(defun zed.last-row (ed)
  (sub1 (count-text-grid-rows (zed.grid ed))))

(defun zed.last-pos? (ed row col)
  (and (= row (zed.last-row ed))
       (= col (zed.last-col ed (zed.last-row ed)))))

;; Return true if the cursor is in the last line displayed, i.e., at the bottom of the editor.
(defun zed.last-displayed-line (ed)
  (+ (zed.first-displayed-line ed) (zed.max-displayed-lines ed)))

(defun zed.last-displayed-column (ed)
  (+ (zed.first-displayed-column ed) (zed.max-displayed-columns ed)))
    
(defun zed.cursor-down (ed)
  (letrec ((row (zed.cursor-row ed))
	   (rowPlus (add1 row)))
    (cond
      ((= row (zed.last-row ed)) (zed.set-cursor-column ed (zed.last-column ed row)))
      (t
       (when (= (add1 row) (zed.last-displayed-line ed)) (zed.scroll-down ed))
       (zed.set-cursor-column ed (min (zed.last-column ed rowPlus) (zed.cursor-column ed)))
       (zed.set-cursor-row ed rowPlus)))))

(defun zed.cursor-up (ed)
  (letrec ((row (zed.cursor-row ed))
	   (rowMin (sub1 row)))
    (cond
      ((= row 0) (void))
      (t
       (when (= row (zed.first-displayed-line ed)) (zed.scroll-up ed))
       (zed.set-cursor-column ed (min (zed.last-column ed rowMin) (zed.cursor-column ed)))
       (zed.set-cursor-row ed rowMin)))))

(defun zed.scroll-down (ed)
  (letrec ((offset (get-scroll-offset (zed.scroll ed)))
	   (woffset (1st offset))
	   (hoffset (2nd offset))
	   (delta-h (2nd (get-text-grid-cell-size (zed.grid ed)))))
    (set-scroll-offset (zed.scroll ed) (list woffset (+ hoffset delta-h)))
    (refresh-object (zed.scroll ed))))

;; scrolls down until the cursor is in the last visible line (this can be used
;; to make sure the editor is scrolled right, e.g. after return key)
(defun zed.scroll-down-to-cursor (ed)
  (letrec ((row (zed.cursor-row ed))
	   (last-row (zed.last-displayed-line ed)))
    (when (> row (sub1 last-row))
      (zed.scroll-down ed)
      (zed.scroll-down-to-cursor ed))))

(defun zed.scroll-up (ed)
  (letrec ((offset (get-scroll-offset (zed.scroll ed)))
	   (woffset (1st offset))
	   (hoffset (2nd offset))
	   (delta-h (2nd (get-text-grid-cell-size (zed.grid ed)))))
    (set-scroll-offset (zed.scroll ed) (list woffset (max 0 (- hoffset delta-h))))
    (refresh-object (zed.scroll ed))))

;; scrolls up until the cursor is about at the half of the editor display
;; (i.e., it doesn't just scroll up minimally but a whole page to make more than
;; one line above visible since this is the more desired behavior)
(defun zed.scroll-up-to-cursor (ed)
  (letrec ((row (zed.cursor-row ed))
	   (first-row (zed.first-displayed-line ed)))
    (when (< row first-row)
      (dotimes (n (/ (zed.max-displayed-lines ed) 2)) (zed.scroll-up ed))
      (zed.scroll-up-to-cursor ed))))

(defun zed.scroll-right (ed)
  (letrec ((offset (get-scroll-offset (zed.scroll ed)))
	   (woffset (1st offset))
	   (hoffset (2nd offset))
	   (delta-w (1st (get-text-grid-cell-size (zed.grid ed)))))
    (set-scroll-offset (zed.scroll ed) (list (+ woffset delta-w) hoffset))
    (refresh-object (zed.scroll ed))))

(defun zed.scroll-left (ed)
   (letrec ((offset (get-scroll-offset (zed.scroll ed)))
	   (woffset (1st offset))
	   (hoffset (2nd offset))
	   (delta-w (1st (get-text-grid-cell-size (zed.grid ed)))))
    (set-scroll-offset (zed.scroll ed) (list (max 0 (- woffset delta-w)) hoffset))
    (refresh-object (zed.scroll ed))))

(defun zed.scroll-left-to-line-start (ed)
  (set-scroll-offset (zed.scroll ed) (list 0 (2nd (get-scroll-offset (zed.scroll ed)))))
  (refresh-object (zed.scroll ed)))

;; scroll to the right to ensure the cursor is visible (should be in the middle of the display)
(defun zed.scroll-right-to-cursor (ed)
  (letrec ((col (zed.cursor-column ed))
	   (offset (get-scroll-offset (zed.scroll ed)))
	   (delta-w (1st (get-text-grid-cell-size (zed.grid ed))))
	   (right-offset (* (max 0 (- col 4)) delta-w)))
    (when (>= col (zed.max-displayed-columns ed))
      (set-scroll-offset (zed.scroll ed) (list right-offset (2nd (get-scroll-offset (zed.scroll ed)))))
      (refresh-object (zed.scroll ed)))))

(defun zed.scroll-left-to-cursor (ed)
   (letrec ((col (zed.cursor-column ed))
	   (offset (get-scroll-offset (zed.scroll ed)))
	   (delta-w (1st (get-text-grid-cell-size (zed.grid ed))))
	   (left-offset (* (max 0 (- col 4)) delta-w)))
    (when (< col (zed.first-displayed-column ed))
      (set-scroll-offset (zed.scroll ed) (list left-offset (2nd (get-scroll-offset (zed.scroll ed)))))
      (refresh-object (zed.scroll ed)))))

(defun zed.cursor-jump-to-line-start (ed)
  (zed.set-cursor-column ed 0)
  (zed.scroll-left-to-line-start ed))

(defun zed.cursor-jump-to-line-end (ed)
  (zed.set-cursor-column ed (zed.last-column ed (zed.cursor-row ed)))
  (zed.scroll-right-to-cursor ed))

(defun zed.cursor-half-page-up (ed)
  (dotimes (n (/ (zed.max-displayed-lines ed) 2)) (zed.cursor-up ed)))

(defun zed.cursor-half-page-down (ed)
  (dotimes (n (/ (zed.max-displayed-lines ed) 2)) (zed.cursor-down ed)))

(defun zed.cursor-home (ed)
  (set-scroll-offset (zed.scroll ed) (list 0 0))
  (zed.set-cursor-row ed 0)
  (zed.set-cursor-column ed 0)
  (refresh-object (zed.scroll ed)))

(defun zed.cursor-end (ed)
  (letrec ((row (zed.last-row ed))
	   (col (zed.last-column ed row)))
    (zed.set-cursor-row ed row)
    (zed.set-cursor-column ed col)
    (set-scroll-offset (zed.scroll ed) (list 0 (2nd (get-object-size (zed.grid ed)))))
    (refresh-object (zed.scroll ed))))

(defun zed.cursor-to-next-word (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (pos (zed.next-word ed row col)))
    (cond
      (pos (zed.set-cursor-row ed (1st pos))
	   (zed.set-cursor-column ed (2nd pos))
	   (if (= row (1st pos))
	       (zed.scroll-right-to-cursor ed)
	       (zed.scroll-left-to-line-start ed)))
      (t (zed.cursor-jump-to-line-end ed)))))

(defun zed.cursor-to-next-half-word (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (pos (zed.next-half-word ed row col)))
    (cond
      (pos (zed.set-cursor-row ed (1st pos))
	   (zed.set-cursor-column ed (2nd pos))
	   (if (= row (1st pos))
	       (zed.scroll-right-to-cursor ed)
	       (zed.scroll-left-to-line-start ed)))
      (t (zed.cursor-jump-to-line-end ed)))))

(defun zed.cursor-to-previous-word (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (pos (zed.previous-word ed row col)))
    (cond
      (pos (zed.set-cursor-row ed (1st pos))
	   (zed.set-cursor-column ed (2nd pos))
	   (if (= row (1st pos))
	       (zed.scroll-left-to-cursor ed)
	       (zed.scroll-right-to-cursor ed)))
      (t (zed.cursor-jump-to-line-start ed)))))

(defun zed.cursor-to-previous-half-word (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (pos (zed.previous-half-word ed row col)))
    (cond
      (pos (zed.set-cursor-row ed (1st pos))
	   (zed.set-cursor-column ed (2nd pos))
	   (if (= row (1st pos))
	       (zed.scroll-left-to-cursor ed)
	       (zed.scroll-right-to-cursor ed)))
      (t (zed.cursor-jump-to-line-start ed)))))

;; advance the cursor by one to the left if it is on a "." or ","
;; for convenience, used on top of certain movement operations
(defun zed.cursor-right-on-punctuation (ed)
   (letrec ((row (zed.cursor-row ed))
	    (col (zed.cursor-column ed))
	    (rune (get-text-grid-rune (zed.grid ed) row col)))
     (when (unicode.is-punct? rune) (zed.cursor-right ed))))

(defun zed.pos-dec (ed row col)
  (if (and (= col 0) (= row 0))
      nil
      (let ((rowMin (sub1 row))
	    (colMin (sub1 col)))
	(cond
	  ((= col 0) (list rowMin (zed.last-column ed rowMin)))
	  (t
	   (list row colMin))))))

(defun zed.pos-inc (ed row col)
  (if (zed.last-pos? ed row col)
      nil
      (let ((rowPlus (add1 row))
	    (colPlus (add1 col)))
	(cond
	  ((= col (zed.last-column ed row)) (list rowPlus 0))
	  (t
	   (list row colPlus))))))

;; true if the rune is a sentence delimiter, used by cursor jumping
(defun zed.delimiter-rune? (rune)
  (case rune
    ((zed.*lf* zed.*soft-lf* "(" ")" "[" "]" "\"" " " "." ";" "," "?" "!") true)
    (t nil)))

;; true if the rune should be considered a delimiter for soft wrap
;; this is different from the previous zed.delimiter-rune? because
;; we want the punctuation in "word." not to break into "word" and "."
(defun zed.soft-wrap-rune? (rune)
  (case rune
    ((zed.*lf* zed.*soft-lf*) true)
    ( t nil)))

;; we're in a word and look for the word's start position
(defun zed.find-word-start (ed row col)
  (let ((pos (zed.pos-dec ed row col)))
    (if (not pos)
	(list row col)
	(letrec ((r (1st pos))
		 (c (2nd pos))
		 (rune (get-text-grid-rune (zed.grid ed) r c)))
	  (cond
	    ((zed.delimiter-rune? rune) (list row col))
	    (t (zed.find-word-start ed r c)))))))

;; we're in a word and look for the word's end position (exclusive, i.e.,
;; the first position after the word)
(defun zed.find-word-end (ed row col)
  (let ((pos (zed.pos-inc ed row col)))
    (if (not pos)
	(list row col)
	(letrec ((r (1st pos))
		 (c (2nd pos))
		 (rune (get-text-grid-rune (zed.grid ed) r c)))
	  (cond
	    ((zed.delimiter-rune? rune) (list r c))
	    (t (zed.find-word-end ed r c)))))))

;; get the start of the next word (returns nil if et end of text)
;; if row and col are inside a word, simply returns row and col
(defun zed.skip-to-next-word (ed row col)
  (cond
    ((zed.delimiter-rune? (get-text-grid-rune (zed.grid ed) row col))
     (let ((pos (zed.pos-inc ed row col)))
       (if (not pos)
	   nil
	   (zed.skip-to-next-word ed (1st pos) (2nd pos)))))
    (t
     (list row col))))

;; get the end of the previous word, nil if there is none
(defun zed.skip-to-previous-word (ed row col)
  (cond
    ((zed.delimiter-rune? (get-text-grid-rune (zed.grid ed) row col))
     (let ((pos (zed.pos-dec ed row col)))
       (if (not pos)
	   nil
	   (zed.skip-to-previous-word ed (1st pos) (2nd pos)))))
    (t
     (list row col))))

;; get the position of the next word: if outside a word, skip to the next word,
;; if inside a word, skip to end, and then to the next word
(defun zed.next-word (ed row col)
  (cond
    ((zed.delimiter-rune? (get-text-grid-rune (zed.grid ed) row col))
     (zed.skip-to-next-word ed row col))
    (t
     (let ((pos (zed.find-word-end ed row col)))
       (if (not pos)
	   nil
	   (zed.skip-to-next-word ed (1st pos)(2nd pos)))))))

;; get the position of the previous word: if outside a word, skip to the previous
;; word and then go to the start, if inside a word, go to start, then to previous word
(defun zed.previous-word (ed row col)
   (cond
    ((zed.delimiter-rune? (get-text-grid-rune (zed.grid ed) row col))
     (let ((pos (zed.skip-to-previous-word ed row col)))
       (if (not pos)
	   nil
	   (zed.find-word-start ed (1st pos)(2nd pos)))))
    (t
     (letrec ((pos (zed.find-word-start ed row col))
	      (pos2 (zed.pos-dec ed (1st pos)(2nd pos))))
       (if (not pos2)
	   nil
	   (let ((pos3 (zed.skip-to-previous-word ed (1st pos2)(2nd pos2))))
	     (if (not pos3)
		 nil
		 (zed.find-word-start ed (1st pos3)(2nd pos3)))))))))

;; get the position of the next word start or the position of the word end,
;; depending on whether row col are inside or outside of a word
(defun zed.next-half-word (ed row col)
   (cond
    ((zed.delimiter-rune? (get-text-grid-rune (zed.grid ed) row col))
     (zed.skip-to-next-word ed row col))
    (t
     (let ((pos (zed.find-word-end ed row col)))
       pos))))

(defun zed.previous-half-word (ed row col)
    (cond
    ((zed.delimiter-rune? (get-text-grid-rune (zed.grid ed) row col))
     (let ((pos (zed.skip-to-previous-word ed row col)))
       (if (not pos)
	   (list 0 0)
	   (zed.find-word-start ed (1st pos)(2nd pos)))))
    (t
     (letrec ((pos (zed.find-word-start ed row col))
	      (pos2 (zed.pos-dec ed (1st pos)(2nd pos))))
       (if (not pos2)
	   pos
	   pos2)))))

;; WORD WRAP

;; soft wrap a string, used when text is inserted initially and not during editing
;; this adds zed.*lf* before "\n" because set-text-grid-text removes trailing "\n"
;; this uses zed.*soft-lf* at the end of a line instead of zed.*lf* as marker for soft wrap
(defun zed.soft-wrap-str (s maxcol)
  (letrec ((lines (strsplit s "\n"))
	   (result ""))
    (foreach lines
	     (lambda (line)
	       (cond
		 ((> (strlen line) maxcol)
		  (letrec ((li (zed.split-line line maxcol))
			   (k (len li)))
		    (dotimes (n k)
		      (cond
			((< n (sub1 k))
			 (setq result (str+ result (nth li n) "\r\n")))
			(t
			 (setq result (str+ result (nth li n) " \n")))))))
		 (t (setq result (str+ result line " \n"))))))
    (slice result 0 (max 0 (- (strlen result) 2)))))

(defun zed.split-line (line maxcol)
  (zed._split-line line maxcol nil))

(defun zed._split-line (line maxcol acc)
  (cond
    ((< (strlen line) maxcol) (reverse (cons line acc)))
    (t
     (let ((splitpoint (zed.find-word-wrap-pos line maxcol)))
       (cond
	 ((= 0 splitpoint)
	  (zed._split-line (slice line maxcol (strlen line)) maxcol (cons (slice line 0 maxcol) acc)))
	 (t 
	  (zed._split-line (slice line splitpoint (strlen line)) maxcol (cons (slice line 0 splitpoint) acc))))))))

(defun zed.find-word-wrap-pos (s pos)
  (let ((rune (slice s pos (add1 pos))))
    (cond
      ((= pos 0) 0)
      ((unicode.is-space? rune) (add1 pos))
      (t
       (zed.find-word-wrap-pos s (sub1 pos))))))

;; find the row in which the paragraph starts in which row is, taking
;; into account potential soft linebreaks zed.*soft-lf* in previous lines
(defun zed.find-para-start-row (ed row)
  (cond
    ((= row 0) 0)
    ((equal? (get-text-grid-rune (zed.grid ed) (sub1 row) (zed.last-column ed (sub1 row)))
	     zed.*lf*) row)
    (t
     (zed.find-para-start-row ed (sub1 row)))))

;;; Selection Handling

(defun zed.paint-selection (ed)
  (let ((sel (zed.selection ed)))
    (when (and sel
	       (>= (len sel) 4))
      (zed.color-range
       ed
       'background
       (1st sel)(2nd sel)(3rd sel)(4th sel)
       (theme-color 'selection)))))

(defun zed.restore-selection-previous (ed)
  (letrec ((d (get (zed.user-dict ed) 'selection-previous nil))
	   (range (1st d nil))
	   (data (2nd d nil)))
    (when range
      (zed.restore-styles-from-range ed (1st range)(2nd range) data))))
    
(defun zed.save-selection-previous (ed range)
  (set (zed.user-dict ed)
       'selection-previous
       (list
	range
	(zed.get-range ed (1st range)(2nd range)(3rd range)(4th range)))))

;; obtain a range as list
(defun zed.get-range (ed start-row start-column end-row end-column)
  (zed._get-range ed start-row start-column end-row end-column nil))

(defun zed._get-range (ed row column end-row end-column acc)
  (cond
    ((or (> row end-row)
	 (> row (zed.last-row ed))
	 (and (= row end-row)
	      (> column end-column)))
     (reverse acc))
    (t (zed._get-range ed (zed.next-row ed row column)
		       (zed.next-col ed row column)
		       end-row end-column
		       (cons (get-text-grid-cell (zed.grid ed) row column) acc)))))

;; restore range
(defun zed.restore-styles-from-range (ed row column data)
  (unless
      (or (null? data)
	  (> row (zed.last-row ed))
	  (and (= row (zed.last-row ed))
	       (> column (zed.last-column (zed.last-row ed) column))))
    (set-text-grid-style (zed.grid ed) row column (2nd (car data) nil))
    (zed.restore-styles-from-range ed (zed.next-row ed row column)
				   (zed.next-col ed row column)
				   (cdr data))))

;; mark a selection, first the start, then the end, or remove the selection
;; if it is already set (similar to Emacs Ctrl-SPACE)
(defun zed.mark-selection-at-cursor (ed)
  (when (and (zed.selection ed)
	     (= (len (zed.selection ed)) 4))
    (zed.remove-selection* ed))
  (let ((sel (zed.selection ed))
	(row (zed.cursor-row ed))
	(col (zed.cursor-column ed)))
    (cond
      ((not sel)
       (zed.set-selection ed (list row col)))
      ((= (len sel) 2)
       (zed.set-selection* ed (append (list (1st sel)(2nd sel))
				      (zed.prev-pos ed (list row col))))))))

;; like remove-selection but takes care of restoring original styles
;; at selection (assuming editor hasn't changed)
(defun zed.remove-selection* (ed)
  (zed.restore-selection-previous ed)
  (zed.remove-selection ed))

;; like set-selection but saves the original style data at the selection
(defun zed.set-selection* (ed sel)
  (zed.save-selection-previous ed sel)
  (zed.set-selection ed sel))
   
;;; Syntax Coloring

(defun zed.paint (ed)
  (foreach (zed.painters ed)
	   (lambda (li)
	     (cond
	       ((and (functional? (2nd li nil))
		     (= (functional-arity (2nd li nil)) 1))
		((2nd li) ed))))))

(defun zed.color-range (ed sort start-row start-column end-row end-column color)
  (case sort
    ((background) (set-text-grid-style-range (zed.grid ed) start-row start-column
					     end-row end-column
					     (list (theme-color 'foreground)
						   color)))
    ((foreground) (set-text-grid-style-range (zed.grid ed) start-row start-column
					     end-row end-column
					     (list color (theme-color 'background))))
    (t (set-text-grid-style-range (zed.grid ed) start-row start-column
				  end-row end-column
				  color))))

(defun zed.remove-colors (ed)
  (set-text-grid-style-range (zed.grid ed) 0 0 (zed.last-row ed)(zed.last-column ed (zed.last-row ed)) nil))

;;; Z3S5 LISP FUNCTIONS
(defun zed.maybe-highlight-parens (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (prevrow (zed.prev-row ed row col))
	   (prevcol (zed.prev-col ed row col)))
    (cond
      ((equal? (get-text-grid-rune (zed.grid ed) row col) "(")
       (zed.paint-highlight-parens ed (list row col) (zed.find-matching-end-paren ed row col)))
      ((equal? (get-text-grid-rune (zed.grid ed) prevrow prevcol) ")")
       (zed.paint-highlight-parens ed (zed.find-matching-start-paren ed prevrow prevcol)(list prevrow prevcol))))))

(defun zed.paint-highlight-parens (ed pos1 pos2)
  (cond
    ((null? pos1)(when pos2 (set-text-grid-style (zed.grid ed) (1st pos2)(2nd pos2) zed.*error-style*)))
    ((null? pos2)(when pos1 (set-text-grid-style (zed.grid ed) (1st pos1)(2nd pos1) zed.*error-style*)))
    ((not (and (null? pos1)
	       (null? pos2)))
     (set-text-grid-style (zed.grid ed)(1st pos1)(2nd pos1) zed.*highlight-paren-style*)
     (set-text-grid-style (zed.grid ed)(1st pos2)(2nd pos2) zed.*highlight-paren-style*))))

(defun zed.find-matching-end-paren (ed row col)
  (zed._find-matching-end-paren ed (zed.next-row ed row col)(zed.next-col ed row col) 1))

(defun zed._find-matching-end-paren (ed row col parens)
  (cond
    ((equal? (get-text-grid-rune (zed.grid ed) row col) ")")
     (if (= parens 1)
	 (list row col)
	 (zed._find-matching-end-paren ed (zed.next-row ed row col)(zed.next-col ed row col)(sub1 parens))))
    ((equal? (get-text-grid-rune (zed.grid ed) row col) "(")
       (zed._find-matching-end-paren ed (zed.next-row ed row col)(zed.next-col ed row col)(add1 parens)))
    ((zed.last-pos? ed row col)
     nil)
    (t (zed._find-matching-end-paren ed (zed.next-row ed row col)(zed.next-col ed row col) parens))))

(defun zed.find-matching-start-paren (ed row col)
  (zed._find-matching-start-paren ed (zed.prev-row ed row col)(zed.prev-col ed row col) 1))

(defun zed._find-matching-start-paren (ed row col parens)
  (cond
    ((equal? (get-text-grid-rune (zed.grid ed) row col) "(")
     (if (= parens 1)
	 (list row col)
	 (zed._find-matching-start-paren ed (zed.prev-row ed row col)(zed.prev-col ed row col)(sub1 parens))))
    ((equal? (get-text-grid-rune (zed.grid ed) row col) ")")
     (zed._find-matching-start-paren ed (zed.prev-row ed row col)(zed.prev-col ed row col)(add1 parens)))
    ((and (= row 0)
	  (= col 0))
     nil)
    (t (zed._find-matching-start-paren ed (zed.prev-row ed row col)(zed.prev-col ed row col) parens))))

(defun zed.lisp-syntax-color-expression (ed row col)
  (let ((start (zed.find-expression-start ed row col)))
    (if start
	(zed.lisp-parse-and-color-expression ed (1st start 0)(2nd start 0) 0)
	(zed.lisp-parse-and-color-expression ed row 0 0))))

(defun zed.lisp-parse-and-color-expression (ed row col parens)
  (letrec ((token (zed.lisp-parse-next-token ed row col parens))
	   (startrow (3rd token))
	   (startcol (4th token))
	   (endpos (zed.prev-pos ed (list (5th token)(6th token))))
	   (endrow (1st endpos))
	   (endcol (2nd endpos))
	   (sym (1st token))
	   (continue t))
    (case sym
      ((lparen rparen)
       (cond
	 ((< (7th token) 0)
	  (set-text-grid-style-range
	   (zed.grid ed)
	   startrow startcol
	   endrow endcol
	   zed.*error-style*))
	 (t (set-text-grid-style-range
	     (zed.grid ed)
	     startrow startcol
	     endrow endcol
	     zed.*paren-style*)))
       (setq continue nil))
      ((comment)
       (set-text-grid-style-range
	(zed.grid ed)
	startrow startcol
	endrow endcol
	zed.*comment-style*))
      ((symbol)
       (let ((s (str->sym (2nd token))))
	 (when (and (zed.help-cb ed)
		    (zed.position-in-range?
		     (list startrow startcol (5th token)(6th token))
		     (list (zed.cursor-row ed)(zed.cursor-column ed))))
	   ((zed.help-cb ed) s))
	 (if (_bound? s)
	     (cond
	       ((macro? (eval s))
		(set-text-grid-style-range
		 (zed.grid ed)
		 startrow startcol
		 endrow endcol
		 zed.*macro-style*))
	       ((functional? (eval s))
		(set-text-grid-style-range
		 (zed.grid ed)
		 startrow startcol
		 endrow endcol
		 zed.*functional-style*))
	       (t (set-text-grid-style-range
		   (zed.grid ed)
		   startrow startcol
		   endrow endcol
		   zed.*symbol-style*)))
	     (if (has-key? zed.*special-forms* s)
		 (set-text-grid-style-range
		  (zed.grid ed)
		  startrow startcol
		  endrow endcol
		  zed.*special-form-style*)
		 (set-text-grid-style-range
		  (zed.grid ed)
		  startrow startcol endrow endcol
		  zed.*unbound-symbol-style*)))))
      ((literal)
       (set-text-grid-style-range
	(zed.grid ed)
	startrow startcol
	endrow endcol
	zed.*literal-style*)))
    (when (and (or continue
		   (> (7th token) 0))
	       (not (zed.last-pos? ed endrow endcol)))
      (zed.lisp-parse-and-color-expression ed (5th token)(6th token)(7th token)))))

(defun zed.is-special-rune? (rune)
  (or (and (unicode.is-space? rune)
	   (not (equal? rune zed.*soft-lf*)))
      (equal? rune "(")
      (equal? rune ")")
      (equal? rune "\"")
      (equal? rune "'")
      (equal? rune "`")
      (equal? rune ";")))

(defun zed.lisp-parse-next-token (ed row col parens)
  (let ((next-row (zed.next-row ed row col))
	(next-col (zed.next-col ed row col))
	(c (get-text-grid-rune (zed.grid ed) row col)))
    (cond 
      ((equal? c "(")(list 'lparen "(" row col next-row next-col (add1 parens)))
      ((equal? c ")")(list 'rparen ")" row col next-row next-col (sub1 parens)))
      ((equal? c ";")(let ((txt (get-text-grid-row-text (zed.grid ed) row)))
		       (list 'comment (slice txt col (max col (sub1 (len txt))))
			     row col
			     row (max col (sub1 (len txt)))
			     parens)))
      ((equal? c "\"")(zed.lisp-parse-string ed row col next-row next-col parens))
      ((not (zed.is-special-rune? c)) (zed.lisp-parse-symbol ed row col parens))
      (t
       (unless (zed.last-pos? ed next-row next-col)
	 (zed.lisp-parse-next-token ed next-row next-col parens))))))
      
(defun zed.lisp-parse-symbol (ed row col parens)
  (let ((pos (zed._parse-until ed row col zed.is-special-rune?)))
    (list 'symbol (zed.get-text-range ed row col (1st pos)(2nd pos))
	  row col (1st pos)(2nd pos) parens)))

(defun zed.lisp-parse-string (ed startrow startcol row col parens)
  (letrec ((pos (zed._parse-until ed row col (lambda (c) (equal? c "\""))))
	   (prev (zed.prev-pos ed pos))
	   (next (zed.next-pos ed pos))
	   (escaped? (equal? (get-text-grid-rune (zed.grid ed) (1st prev)(2nd prev)) "\\")))
      (cond
      ((zed.last-pos? ed row col)
       (if escaped?
	   (list 'incomplete-string (zed.get-text-range ed startrow startcol (1st pos) (2nd pos)) startrow startcol
		 (1st next) (2nd next) parens)
	   (list 'literal (zed.get-text-range ed startrow startcol (1st next) (2nd next)) startrow startcol
		 (1st next) (2nd next) parens)))
      (t
       (if escaped?
	   (zed.lisp-parse-string ed startrow startcol (1st next)(2nd next) parens)
	   (list 'literal (zed.get-text-range ed startrow startcol (1st next) (2nd next)) startrow startcol
		 (1st next) (2nd next) parens))))))
    
  
;; get the text range from row col (inclusive) and end-row end-col (exclusive), where soft line breaks have been
;; removed
(defun zed.get-text-range (ed row col end-row end-col)
  (zed._get-text-range ed (zed.grid ed) row col end-row end-col ""))

(defun zed._get-text-range (ed grid row col end-row end-col acc)
  (if (and (= row end-row)
	   (= col end-col))
      acc
      (let ((c (get-text-grid-rune grid row col)))
	(if (equal? c zed.*soft-lf*)
	    (zed._get-text-range ed grid (zed.next-row ed row col)(zed.next-col ed row col) end-row end-col acc)
	    (zed._get-text-range ed grid (zed.next-row ed row col)(zed.next-col ed row col) end-row end-col (str+ acc c))))))

;; parse until the predicate is non-nil, return the end position (exclusive, i.e., the first
;; position where pred returns non-nil)
(defun zed._parse-until (ed row col pred)
  (if (or (zed.last-pos? ed row col)
	  (pred (get-text-grid-rune (zed.grid ed) row col)))
      (list row col)
      (zed._parse-until ed (zed.next-row ed row col)(zed.next-col ed row col) pred)))

(defun zed.syntax-color-comment (ed row col)
  (zed.style-to-end-of-line ed row col zed.*comment-style*))

(defun zed.style-to-end-of-line (ed row col style)
  (dotimes (n (add1 (- (zed.last-column ed row) col)))
    (set-text-grid-style (zed.grid ed) row (sub1 (+ col n)) style)))

;; approximate the range of an expression at the given position
;; this function assumes the expression starts on a separate new line
;; and ends in a separate line
;; uses parse-expressions to get the ranges of all expressions in the buffer
;; Todo: This is inefficient if we really just need the expression under the cursor.
(defun zed.find-lisp-expression-at (ed row col)
  (letrec ((expr (zed.parse-expressions ed 0 0 row col nil 0 nil))
	   (traverse (lambda (li acc)
		       (cond
			 ((null? li) acc)
			 (t
			  (let ((elem (1st li nil)))
			    (cond
			      ((null? elem) nil)
			      ((and (>= row (1st elem))
				  (>= col (2nd elem))
				  (not (null? (cddr elem)))
				  (or (< row (3rd elem))
				      (and (= row (3rd elem))
					   (<= col (4th elem)))))
			       elem)
			      (t (traverse (cdr li) acc)))))))))
    (traverse expr nil)))

(defun zed.find-expression-start (ed row col)
  (letrec ((loop
	      (lambda (row)
		(cond
		  ((< row 0) nil)
		  ((equal? (get-text-grid-rune (zed.grid ed) row 0) ";")
		   (list row 0))
		  ((and (zed.line-is-sexpr-start? ed row)
			(zed.line-is-comment-or-empty? ed (sub1 row)))
		   (list row 0))
		  (t (loop (sub1 row)))))))
    (loop row)))

(defun zed.line-is-comment-or-empty? (ed row)
  (cond
    ((< row 0) t)
    ((= (zed.last-column ed row) 0) t)
    ((equal? (get-text-grid-rune (zed.grid ed) row 0) 
	     ";") true)
    (t nil)))

(defun zed.line-is-sexpr-start? (ed row)
  (equal? (get-text-grid-rune (zed.grid ed) row 0)
	  "("))
    
;; return the next row, taking into account limits
(defun zed.next-row (ed row col)
  (cond
    ((< col (zed.last-column ed row)) row)
    ((< row (zed.last-row ed)) (add1 row))
    (t row)))

;; return the next column, taking into account limits
(defun zed.next-col (ed row col)
  (cond
    ((< col (zed.last-column ed row)) (add1 col))
    (t 0)))

;; return true if the position is in the given range, nil otherwise
(defun zed.position-in-range? (range pos)
  (and (>= (1st pos)(1st range))
       (<= (1st pos)(3rd range))
       (not (and (= (1st pos)(1st range))
		 (< (2nd pos)(2nd range))))
       (not (and (= (1st pos)(3rd range))
		 (> (2nd pos)(4th range))))))
  

;; return the next (row col) position for input list (row col)
;; returns the last position (not nil) as fixed point,
;; if pos is the last position
(defun zed.next-pos (ed pos)
  (list (zed.next-row ed (1st pos)(2nd pos))
	(zed.next-col ed (1st pos)(2nd pos))))

(defun zed.prev-row (ed row col)
  (if (= col 0)
      (max 0 (sub1 row))
      row))

(defun zed.prev-col (ed row col)
  (if (= col 0)
      (zed.last-column ed (max 0 (sub1 row)))
      (sub1 col)))

;; return the previous (row col) position for input list (row col)
;; returns (0 0) as fixed point (not nil), if pos is (0 0)
(defun zed.prev-pos (ed pos)
      (list (zed.prev-row ed (1st pos)(2nd pos))
	    (zed.prev-col ed (1st pos)(2nd pos))))

;; find all s-expressions in the buffer, return a list of lists (start-row start-col end-row end-col)
(defun zed.parse-expressions (ed current-row current-col end-row end-col acc open-paren last-rune)
  (let ((rune (get-text-grid-rune (zed.grid ed) current-row current-col)))
    (cond
      ((zed.last-pos? ed current-row current-column)
       acc)
      ((equal? rune "(")
       (cond
	 ((= 0 open-paren)
	  (zed.parse-expressions
	   ed
	   (zed.next-row ed current-row current-col)
	   (zed.next-col ed current-row current-col)
	   end-row end-col (cons (list current-row current-col) acc)
	   (add1 open-paren) rune))
	 (t 
	  (zed.parse-expressions
	   ed
	   (zed.next-row ed current-row current-col)
	   (zed.next-col ed current-row current-col)
	   end-row end-col acc
	   (add1 open-paren) rune))))
      ((equal? rune ")")
       (cond
	 ((= (sub1 open-paren) 0 )
	  (zed.parse-expressions
	   ed
	   (zed.next-row ed current-row current-col)
	   (zed.next-col ed current-row current-col)
	   end-row end-col (cons (append (1st acc nil) (list current-row current-col)) (cdr acc))
	   (sub1 open-paren) rune))
	 (t 
	  (zed.parse-expressions
	   ed
	   (zed.next-row ed current-row current-col)
	   (zed.next-col ed current-row current-col)
	   end-row end-col acc
	   (sub1 open-paren) rune))))
      ((equal? rune ";")
       (cond
	 ((< (zed.last-row ed) current-row)
	  (zed.parse-expressions-until
	   ed
	   (add1 current-row)
	   0 end-row end-col acc
	   open-paren rune))
	 (t acc)))
      (t (zed.parse-expressions
	  ed
	  (zed.next-row ed current-row current-col)
	  (zed.next-col ed current-row current-col)
	  end-row end-col acc open-paren rune)))))

(defun zed.create-lorem-ipsum-content (n)
  (letrec ((fn
	    (lambda (i s)
	      (cond
		((= i 0) s)
		(t (fn
		    (sub1 i)
		    (str+ s
			  (create-lorem-ipsum 'paragraph 2 20)
			  "\n\n")))))))
    (fn n "")))

;;; TESTING
(defun get-help-string (sym)
  (let ((h (help-entry sym)))
    (if h
	(str+
	 (assoc1 'use h)
	 "\n\n"
	 (str-replace* (assoc1 'info h) "#" "")
	 "\n\n"
	 (fmt "See also: %v" (assoc1 'see h)))
	(fmt "%v\n\nNo help available." sym))))

(defun zed.test ()
  (letrec ((win (new-window "Editor"))
	   (canvas (get-window-canvas win))
	   (ed (zed.new '(soft-wrap)))
	   (e (new-entry 'multi-line))
	   (split (new-hsplit (zed.scroll ed) e)))
    (zed.set-text
     ed
     (str+ ";; return the next column, taking into account limits\n"
	   "(defun zed.next-col (ed row col)\n"
	   "  (cond\n"
	   "    ((< col (zed.last-column ed row)) (add1 col))\n"
	   "    (t 0)))\n"
	   "\n"
	   ";; return the next (row col) position for input list (row col)\n"
	   ";; returns the last position (not nil) as fixed point,\n"
	   ";; if pos is the last position\n"
	   "(defun zed.next-pos (ed pos)\n"
	   "  (list (zed.next-row ed (1st pos)(2nd pos))\n"
	   "	(zed.next-col ed (1st pos)(2nd pos))))\n"
	   "\n"
	   "(setq a \"This is a test\")\n\n"))
    (set-entry-text-wrap e 'word)
    (zed.add-painter ed 'lisp (lambda (ed) (zed.lisp-syntax-color-expression ed (zed.cursor-row ed)(zed.cursor-column ed))))
    (zed.install-key-handler ed canvas)
    (zed.install-rune-handler ed canvas)
    (zed.set-help-cb ed (lambda (sym) (when (help-entry sym) (set-entry-text e (get-help-string sym)))))
    (set-split-offset split 0.6)
    (set-window-content win split)
    (set-window-size win 1100 600)
    (zed.install-default-shortcuts ed canvas)
    (show-window win)))

