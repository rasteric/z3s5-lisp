;;;; Z3S5 Lisp editor

(setq zed.*blink-cursor-on-interval* 400)
(setq zed.*blink-cursor-off-interval* 50)

(defun zed.new ()
   (let ((grid (new-text-grid 'show-whitespace)))
    (letrec ((ed (array 'zed.editor grid 0 0 true true (make-mutex))))
      (zed.set-text ed "")
      (future (letrec ((blink (lambda ()
				(when (zed.draw-cursor? ed)
				  (zed.set-cursor-state ed (not (zed.cursor-state ed)))
				  (zed.draw-cursor ed (zed.cursor-state ed)))
				(if (zed.cursor-state ed)
				    (sleep zed.*blink-cursor-on-interval*)
				    (sleep zed.*blink-cursor-off-interval*))
				(blink))))
		(blink)))
      ed)))

(defun zed.is-editor? (ed)
  (and (array? ed)
       (> (array-len ed) 0)
       (equal? (array-ref ed 0) 'zed.editor)))

(defun zed.grid (ed)
  (array-ref ed 1))

(defun zed.cursor-row (ed)
  (array-ref ed 2))

(defun zed.set-cursor-row (ed row)
  (array-set ed 2 row))

(defun zed.set-cursor-column (ed col)
  (array-set ed 3 col))

(defun zed.cursor-column (ed)
  (array-ref ed 3))

(defun zed.draw-cursor? (ed)
  (array-ref ed 4))

(defun zed.set-draw-cursor (ed on?)
  (array-set ed 4 on?))

(defun zed.cursor-state (ed)
  (array-ref ed 5))

(defun zed.set-cursor-state (ed on?)
  (array-set ed 5 on?))

(defun zed.mutex (ed)
  (array-ref ed 6))

(defun zed.draw-cursor (ed on?)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (style (2nd (get-text-grid-cell (zed.grid ed) row col) nil))
	   (fgcolor (zed.style-fg-color style))
	   (bgcolor (zed.style-bg-color style)))
    (set-text-grid-style
     (zed.grid ed)
     row
     col
     (if on?
	 (list bgcolor fgcolor)
	 (list fgcolor bgcolor)))))

(defun zed.invert-color (li)
  (let ((r (1st li))
	(g (2nd li))
	(b (3rd li))
	(a (4th li 255)))
    (list (- 255 r) (- 255 g) (- 255 b) a)))

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
  (out key)(out "\n")
  (case key
    ((left) (zed.cursor-left ed))
    ((right) (zed.cursor-right ed))
    ((up) (zed.cursor-up ed))
    ((down) (zed.cursor-down ed))))

(defun zed.install-key-handler (ed canvas)
  (set-canvas-on-typed-key
   canvas (lambda (key code)
	    (zed.key-handler ed key code))))

(defun zed.cursor-on (ed)
  (zed.set-draw-cursor ed true))

(defun zed.cursor-off (ed)
  (zed.set-draw-cursor ed nil)
  (zed.draw-cursor ed nil))
 
(defun zed.cursor-left (ed)
  (zed.cursor-off ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (rowMinus (sub1 row))
	   (colMinus (sub1 col)))
    (cond
      ((and (= row 0) (= col 0)) (void))
      ((> col 0) (zed.set-cursor-column ed colMinus))
      (t
       (zed.set-cursor-column ed (zed.last-column ed rowMinus))
       (zed.set-cursor-row ed rowMinus))))
  (zed.cursor-on ed))

(defun zed.cursor-right (ed)
  (zed.cursor-off ed)
  (letrec ((row (zed.cursor-row ed))
	   (col (zed.cursor-column ed))
	   (rowPlus (add1 row))
	   (colPlus (add1 col)))
    (cond
      ((and (= row (zed.last-row ed))
	    (= col (zed.last-column ed row)))
       (void))
      ((< colPlus (zed.last-column ed row))
       (zed.set-cursor-column ed colPlus))
      (t
       (when (< rowPlus (zed.last-row ed))
	 (zed.set-cursor-column ed 0)
	 (zed.set-cursor-row ed rowPlus)))))
  (zed.cursor-on ed))

(defun zed.last-column (ed row)
  (sub1 (count-text-grid-row-columns (zed.grid ed) row)))

(defun zed.last-row (ed)
  (sub1 (count-text-grid-rows (zed.grid ed))))

(defun zed.cursor-down (ed)
  (zed.cursor-off ed)
  (letrec ((row (zed.cursor-row ed))
	   (rowPlus (add1 row)))
    (cond
      ((= row (zed.last-row ed)) (void))
      (t
       (zed.set-cursor-column ed (min (zed.last-column ed rowPlus) (zed.cursor-column ed)))
       (zed.set-cursor-row ed rowPlus))))
  (zed.cursor-on ed))

(defun zed.cursor-up (ed)
  (zed.cursor-off ed)
  (letrec ((row (zed.cursor-row ed))
	   (rowMin (sub1 row)))
    (cond
      ((= row 0) (void))
      (t
       (zed.set-cursor-column ed (min (zed.last-column ed rowMin) (zed.cursor-column ed)))
       (zed.set-cursor-row ed rowMin))))
  (zed.cursor-on ed))

(defun zed.test ()
  (letrec ((win (new-window "Editor"))
	   (ed (zed.new)))
    (zed.set-text ed "(defun zed.draw-cursor (ed on?)\n  (letrec ((row (zed.cursor-row ed))\n	   (col (zed.cursor-column ed))\n	   (style (2nd (get-text-grid-cell (zed.grid ed) row col) nil))\n	   (fgcolor (zed.style-fg-color style))\n	   (bgcolor (zed.style-bg-color style)))\n    (set-text-grid-style      (zed.grid ed)\n     row\n     col\n     (if on?\n	 (list bgcolor fgcolor)\n	 (list fgcolor\n bgcolor)))))")
    (zed.install-key-handler ed (get-window-canvas win))
    (set-window-content win (zed.grid ed))
    (set-window-size win 600 400)
    (show-window win)))

