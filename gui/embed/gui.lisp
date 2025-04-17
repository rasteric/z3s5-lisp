(defun color->color64 (c)
  (map c (lambda (x) (* x 257))))

(defun color64->color (c)
  (map c (lambda (x) (int (/ x 257)))))

(defmacro gui (&rest body)
 `(_gui (lambda () ,@body)))

(defmacro gui+ (&rest body)
 `(_gui+ (lambda () ,@body)))

(defmacro gui* (&rest body)
 `(_gui* (lambda () ,@body)))
