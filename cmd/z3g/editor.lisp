;;;; Z3S5 Lisp editor

(setq zed.*blink-cursor-on-interval* 800)
(setq zed.*blink-cursor-off-interval* 200)

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

(defun zed.new ()
  (let ((grid (new-text-grid)))
    (letrec ((ed (array 'zed.editor grid 0 0 true true (make-mutex) (dict))))
      (zed.set-text ed "")
      (zed.blink-cursor ed)
      ed)))

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

;; 7 style at a given row and column, used for restoring to normal
(defun zed.get-style (ed row column)
  (get-or-set (array-ref ed 7) (fmt "%v:%v" row column)
	      (list (theme-color 'foreground) (theme-color 'input-background))))

(defun zed.save-style (ed row column style)
  (set (array-ref ed 7) (fmt "%v:%v" row column) style))

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
;	(ed.save-style ed row col style)
	(list (zed.invert-color fgcolor) fgcolor))
       (t
					;	(zed.get-style ed row col))))
	(list (theme-color 'foreground) (theme-color 'background)))))
    (refresh-object (zed.grid ed)))) 

(defun zed.invert-color (li)
  (let ((r (1st li))
	(g (2nd li))
	(b (3rd li))
	(a (4th li 65535)))
    (list (- 65535 r) (- 65535 g) (- 65535 b) a)))

(defun zed.set-text (ed s)
  (set-text-grid-text (zed.grid ed) (zed.add-internal-line-endings s)))

(defun zed.get-text (ed)
  (zed.remove-internal-line-endings (get-text-grid-text (zed.grid ed))))

(defun zed.add-internal-line-endings (s)
  (let ((a (strsplit s "\n"))
	(result ""))
    (array-map! a (lambda (x) (str+ x " ")))
    (dotimes (n (array-len a))
      (if (= n (sub1 (array-len a)))
	  (setq result (str+ result (array-ref a n)))
	  (setq result (str+ result (array-ref a n) "\n"))))
    result))

(defun zed.remove-internal-line-endings (s)
  (let ((a (strsplit s "\n"))
	(result ""))
    (array-map! a (lambda (x) (slice x 0 (len x))))
    (dotimes (n (array-len a))
      (if (= n (sub1 (array-len a)))
	  (setq result (str+ result (array-ref a n)))
	  (setq result (str+ result (array-ref a n) "\n"))))
    result))

(defun zed.style-fg-color (s)
  (1st s '(0 0 0 255)))

(defun zed.style-bg-color (s)
  (2nd s '(255 255 255 255)))

(defun zed.key-handler (ed key code)
  (zed.draw-cursor ed nil)
  (case key
    ((left) (zed.cursor-left ed))
    ((right) (zed.cursor-right ed))
    ((up) (zed.cursor-up ed))
    ((down) (zed.cursor-down ed)))
  (zed.lisp-syntax-color-at-expression ed)
  (zed.draw-cursor ed true))

(defun zed.install-key-handler (ed canvas)
  (set-canvas-on-typed-key
   canvas (lambda (key code)
	    (zed.lock ed)
	    (zed.key-handler ed key code)
	    (zed.unlock ed))))
 
(defun zed.cursor-left (ed)
   (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (rowMinus (sub1 row))
	   (colMinus (sub1 col)))
    (cond
      ((and (= row 0) (= col 0)) (void))
      ((> col 0) (zed.set-cursor-column ed colMinus))
      (t
       (zed.set-cursor-column ed (zed.last-column ed rowMinus))
       (zed.set-cursor-row ed rowMinus)))))

(defun zed.cursor-right (ed)
   (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (rowPlus (add1 row))
	   (colPlus (add1 col)))
    (cond
      ((and (= row (zed.last-row ed))
	    (= col (zed.last-column ed row)))
       (void))
      ((<= colPlus (zed.last-column ed row))
       (zed.set-cursor-column ed colPlus))
      (t
       (when (<= rowPlus (zed.last-row ed))
	 (zed.set-cursor-column ed 0)
	 (zed.set-cursor-row ed rowPlus))))))

(defun zed.last-column (ed row)
  (sub1 (count-text-grid-row-columns (zed.grid ed) row)))

(defun zed.last-row (ed)
  (sub1 (count-text-grid-rows (zed.grid ed))))

(defun zed.cursor-down (ed)
  (letrec ((row (zed.cursor-row ed))
	   (rowPlus (add1 row)))
    (cond
      ((= row (zed.last-row ed)) (zed.set-cursor-column ed (zed.last-column ed row)))
      (t
       (zed.set-cursor-column ed (min (zed.last-column ed rowPlus) (zed.cursor-column ed)))
       (zed.set-cursor-row ed rowPlus)))))

(defun zed.cursor-up (ed)
  (letrec ((row (zed.cursor-row ed))
	   (rowMin (sub1 row)))
    (cond
      ((= row 0) (void))
      (t
       (zed.set-cursor-column ed (min (zed.last-column ed rowMin) (zed.cursor-column ed)))
       (zed.set-cursor-row ed rowMin)))))

;;; Z3S5 LISP FUNCTIONS

(defun zed.lisp-syntax-color-at-expression (ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (range (zed.find-lisp-expression-at ed row col)))
    (unless (< (len range) 4)
      (zed.lisp-syntax-color-range ed (1st range) (2nd range) (3rd range) (4th range)))))

(defun zed.lisp-syntax-color-range (ed start-row start-col end-row end-col)
  (out (fmt "%v, %v:%v, %v\n" start-row start-col end-row end-col)))

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

;; find all s-expressions in the buffer, return a list of lists (start-row start-col end-row end-col)
(defun zed.parse-expressions (ed current-row current-col end-row end-col acc open-paren last-rune)
  (let ((rune (get-text-grid-rune (zed.grid ed) current-row current-col)))
    (cond
      ((and (= current-row (zed.last-row ed))
	    (= current-col (zed.last-column ed current-row)))
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

;;; TESTING
(defun zed.test ()
  (letrec ((win (new-window "Editor"))
	   (ed (zed.new)))
    (zed.set-text ed "(defun zed.draw-cursor (ed on?)\n  (letrec ((row (zed.cursor-row ed))\n	   (col (zed.cursor-column ed))\n	   (style (2nd (get-text-grid-cell (zed.grid ed) row col) nil))\n	   (fgcolor (zed.style-fg-color style))\n	   (bgcolor (zed.style-bg-color style)))\n    (set-text-grid-style\n      (zed.grid ed)\n     row\n     col\n     (if on?\n	 (list bgcolor fgcolor)\n	 (list fgcolor bgcolor)))))\n\n(defun mult (x y)\n    (* x y))")
    (zed.install-key-handler ed (get-window-canvas win))
    (set-window-content win (zed.grid ed))
    (set-window-size win 600 400)
    (show-window win)))

