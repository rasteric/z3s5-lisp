(defun color->color64 (c)
  (map c (lambda (x) (* x 257))))

(defun color64->color (c)
  (map c (lambda (x) (int (/ x 257)))))
