;;; This file is embedded and loaded at startup. It is best not to modify it
;;; and instead use a dynamic init.lisp file from the file system or embed
;;; your own file into an application using Z3S5 Lisp as a package.

(defmacro when-permission (perm &rest body)
  `(cond
     ((permission? ,perm) ,@body)))

(defun out (s)
 (princ s))

(defun protect (&rest symbols)
  (cond
    ((permission? 'allow-protect) (list-foreach symbols (lambda (x) (_protect x))))
    (t (error "protect: security violation - no permission to protect symbols!"))))

(defun unprotect (&rest symbols)
  (cond
    ((permission? 'allow-unprotect) (list-foreach symbols (lambda (x) (_unprotect x))))
    (t (error "unprotect: security violation - no permission to unprotect symbols!"))))

(declare-unprotected '*gensym-counter*)

(setq true t)
(setq false nil)

(defmacro case (test &rest cases)
  (letrec ((sym (gensym))
	   (clauses (mapcar cases
			    (lambda (x)
			      (cond ((or (equal? (car x) 'true)
					 (equal? (car x) 't)
					 (equal? (car x) 'else))
				     x)
				    ((atom? (car x))
				     `((equal? ,sym ',(car x)) ,@(cdr x)))
				    (t
				     `((member ,sym ',(car x)) ,@(cdr x))))))))
    `(let ((,sym ,test))
       (cond ,@clauses))))

(defmacro bound? (sym)
  (_bound? sym))

(defun proc? (sym)
  (or (intrinsic? sym) (closure? sym)))

(defun functional? (sym)
  (or (proc? sym) (macro? sym)))

(setq EOF (end-of-file))

(defmacro when-permission (perm &rest body)
  `(cond
     ((permission? ,perm) ,@body)))


;; ESSENTIAL MACROS AND FUNCTIONS

(defun dfc! (sym)
  (progn (bind sym 0) sym))

(defmacro on-feature (test &rest body)
  `(when (feature? ,test)
     ,@body))


;; TESTING

(defun init-testing ()
  (setq *tests* nil)(declare-unprotected '*tests*)
  (setq *testinfo* (dict)))

(defmacro testing (name)
  `(setq *tests*
	 (cons 
	  (lambda ()
	    (_testout ,name)(_testout " ")
	    (set *testinfo* 'test ,name))
	  *tests*)))

(defun _testidx ()
  (get *testinfo* 'count 0))

(defun _testout (arg)
  (unless (get *testinfo* 'silent? nil)
    (out arg)))

(defun _testouty (arg)
  (unless (get *testinfo* 'silent? nil)
    (outy arg)))

(defmacro expect (value computed)
  (letrec ((sym (gensym)))
    `(setq *tests*
	   (cons
	    (lambda ()
	      (set *testinfo* 'count (add1 (get *testinfo* 'count 0)))
	      (let ((,sym ,computed))
		(cond
		  ((equal? ,sym ,value)
		   (_testout (fmt "%v " (_testidx)))
		   (set *testinfo* 'success (add1 (get *testinfo* 'success 0))))
		  (t
		   (_testouty (list
			       (list 'fg 'z3s5-error-text
				     (list 'bg 'z3s5-error-back
					   (list 'text
						 (shorten (fmt "\n%v FAIL, expect %v given %v\n"
							       (_testidx) ,value ,sym) (sys 'cols 80)))))))
		   (set *testinfo* 'failure (add1 (get *testinfo* 'failure 0)))))))
	    *tests*))))

(defmacro expect-true (&rest computed)
  (letrec ((sym (gensym)))
    `(setq *tests*
	   (cons
	    (lambda ()
	      (set *testinfo* 'count (add1 (get *testinfo* 'count 0)))
	      (let ((,sym (progn ,@computed)))
		(cond
		  (,sym
		   (_testout (fmt "%v " (_testidx)))
		   (set *testinfo* 'success (add1 (get *testinfo* 'success 0))))
		  (t
		   (_testouty (list
			       (list 'fg 'z3s5-error-text
				     (list 'bg 'z3s5-error-back
					   (list 'text
						 (shorten (fmt "\n%v FAIL, expect true, result is %v\n"
							       (_testidx) ,sym) (sys 'cols 80)))))))
		   (set *testinfo* 'failure (add1 (get *testinfo* 'failure 0)))))))
	    *tests*))))

(defmacro expect-false (&rest computed)
  (letrec ((sym (gensym)))
    `(setq *tests*
	   (cons
	    (lambda ()
	      (set *testinfo* 'count (add1 (get *testinfo* 'count 0)))
	      (let ((,sym (progn ,@computed)))
		(cond
		  ((not ,sym)
		   (_testout (fmt "%v " (_testidx)))
		   (set *testinfo* 'success (add1 (get *testinfo* 'success 0))))
		  (t
		   (_testouty (list
			       (list 'fg 'z3s5-error-text
				     (list 'bg 'z3s5-error-back
					   (list 'text
						 (shorten (fmt "\n%v FAIL, expect true, result is %v\n"
							       (_testidx) ,sym) (sys 'cols 80)))))))
		   (set *testinfo* 'failure (add1 (get *testinfo* 'failure 0)))))))
	    *tests*))))

(defmacro expect-err (&rest computed)
  `(setq *tests*
	 (cons
	  (lambda ()
	    (set *testinfo* 'count (add1 (get *testinfo* 'count 0)))
	    (_testout (fmt "%v " (get *testinfo* 'count 0)))
	    (push-error-handler
	     (lambda (err)
	       (pop-error-handler)
	       (set *testinfo* 'success (add1 (get *testinfo* 'success 0)))
	       (let ((continuation (get *testinfo* 'continuation nil)))
		 (when continuation (continuation)))))
	    ,@computed
	    (set *testinfo* 'failure (add1 (get *testinfo* 'failure 0)))
	    (_testouty (list
			(list 'fg 'z3s5-error-text
			      (list 'bg 'z3s5-error-back
				    (list 'text
					  (fmt "\n%v FAIL, expect error but none occurred\n"
					       (_testidx)))))))
	    (pop-error-handler))
	  *tests*)))

(defmacro expect-ok (&rest computed)
  `(setq *tests*
	 (cons
	  (lambda ()
	    (set *testinfo* 'count (add1 (get *testinfo* 'count 0)))
	    (push-error-handler
	     (lambda (err)
	       (pop-eror-handler)
	       (set *testinfo* 'failure (add1 (get *testinfo* 'failure 0)))
	       (let ((continuation (get *testinfo* 'continuation nil)))
		 (when continuation (continuation)))))
	    ,@computed
	    (set *testinfo* 'success (add1 (get *testinfo* 'success 0)))
	    (_testout (fmt "%v " (_testidx)))
	    (pop-error-handler))
	  *tests*)))

;; This is a hack. Bear in mind that Z3S5 Series A lisp does not give us continuations
;; after an error, i.e., when the error handler is called and executed, program flow will
;; return to the eval-print-loop or the program might terminate if we're not running the system
;; interactively. So we store the continuation, which is called both when no error occurs
;; and when an error occurs. Otherwise traversing the tests would stop at the first
;; error. It would be nice to have a general macro for this, something like dynamic-wind.
(defun run-selftest (&rest opt)
  (set *testinfo* 'silent? (1st opt nil))
  (unless (get *testinfo* 'silent?) (cls))
  (_testouty `((fg z3s5-blue
		   (bg z3s5-orange
		       (text ,(strcenter "<<< RUNNING Z3S5 MACHINE SERIES A SELFTEST >>>" (sys 'cols 80)))))))
  (unless (get *testinfo* 'silent? nil) (beep 'info))
  (set *testinfo* 'count 0)
  (set *testinfo* 'failure 0)
  (set *testinfo* 'success 0)
  (set *testinfo* 'error 0)
  (letrec ((tests (reverse *tests*))
	   (traverse (lambda ()
		       (cond
			 ((null? tests) (void))
			 (t
			  (let ((test (car tests)))
			    (setq tests (cdr tests))
			    (test)
			    (traverse))))))
	   (continue (lambda ()
		       (pop-error-handler)
		       (set *testinfo* 'continuation nil)
		       (let ((perfect? (and (= (get *testinfo* 'failure 0) 0)
					    (= (get *testinfo* 'error 0) 0))))
			 (unless (get *testinfo* 'silent? nil)
			   (if perfect?
			       (void (future (dotimes (n 3) (beep 'okay)(sleep 500))))
			       (void (future (dotimes (n 3) (beep 'error) (sleep 400))))))
			 (let ((msg (fmt "\n==> Passed %v of %v tests, %v errors, %v failures.\n"
					 (get *testinfo* 'success)
					 (get *testinfo* 'count)
					 (get *testinfo* 'error)
					 (get *testinfo* 'failure))))
			   (if perfect?
			       (_testout msg)
			       (_testouty `((bg z3s5-error-back
						(fg z3s5-error-text
						    (text ,msg)))))))))))
    (set *testinfo* 'continuation (lambda () (traverse)(continue)))
    (push-error-handler
     (lambda (err)
       (set *testinfo* 'error (add1 (get *testinfo* 'error 0)))
       (_testouty (list
		   (list 'fg 'z3s5-error-text
			 (list 'bg 'z3s5-error-back
			       (list 'text
				     (str+ (shorten (fmt "\n%v ERR %v"
							 (get *testinfo* 'count 0) err) (* 3 (sys 'cols 80)))
					   "\n"))))))
       (let ((continuation (get *testinfo* 'continuation nil)))
	 (when continuation (continuation)))))
    (traverse)
    (continue)))

(init-testing)

(testing "testing")
(expect-true t)
(expect-false nil)
(expect-ok (void))
(expect-err (error "intentional error for testing"))

;; HELP SYSTEM
(testing "help")

(setq *_current-lib* nil)
(declare-unprotected '*_current-lib*)

(when (bound? *reflect*)
  (setq *reflect* (cons 'help *reflect*)))

(defun _push-current-lib (lib)
  (setq *_current-lib* (cons lib *_current-lib*)))

(defun _pop-current-lib ()
  (void (setq *_current-lib* (cdr *_current-lib*))))

(setq *help*
      (dict '(help-entry
	      ((use "(help-entry sym) => list")
	       (info "Get usage and help information for #sym.")
	       (type proc)
	       (arity 1)
	       (result list)
	       (see (defhelp help apropos *help*))))))

(defun help-entry (sym)
  (get *help* sym nil))

(defmacro defhelp (sym &rest entries)
  `(_set-help-entries ',sym ',entries))

(defmacro help (sym)
  `(cond
     ((has-key? *help* ',sym) (_out-help ',sym (get *help* ',sym nil)))
     (t (outy
	 (list
	  (list 'fg 'z3s5-help-text
		(list 'bg 'z3s5-help-back
		      (fmt "Sorry, no help is available for '%v.\n" ',sym))))))))


(defun _set-help-entries (sym ent)
  (cond
    (*_current-lib*
     (let ((newstr (_prefix-lib-sym *_current-lib* sym)))
       (set *help* (str->sym newstr) (_transform-help-entries newstr (sym->str sym) ent))))
    (t (set *help* sym ent))))

;; transform with prefix, for use with *current-lib* when using (load 'lib)
;; '(strings algo) 'boyer-more => "algo.strings.boyer-more"
(defun _prefix-lib-sym (prefixli oldsym)
  (str+
   (str-join
    (mapcar (reverse prefixli) sym->str) ".")
   "."
   (sym->str oldsym)))

(defun _transform-help-entries (strnew strold entries)
  (mapcar entries
	  (lambda (entry)
	    (cons (car entry)
		  (mapcar (cdr entry) (lambda (x) (_maybe-translate-help x strnew strold)))))))

(defun _maybe-translate-help (datum strnew strold)
  (cond
    ((sym? datum)
     (let ((dstr (sym->str datum)))
       (cond
	 ((equal? dstr strold)
	  (str->sym strnew))
	 ((equal? (1st dstr 0) 46) ; ".symbol" -> "prefix.symbol"
	  (let ((without-dot (str->sym (slice dstr 1 (len dstr)))))
	  (if *_current-lib* 
	      (str->sym (_prefix-lib-sym *_current-lib* without-dot)) ; _prefix-lib-sym adds the "."
	      without-dot)))
	 (t datum))))
    ((str? datum)
     (str-replace datum strold strnew -1))
    ((and (list? datum) (not (null? datum)))
     (cons (_maybe-translate-help (car datum) strnew strold)
	   (_maybe-translate-help (cdr datum) strnew strold)))
    (t datum)))

(defhelp help
    (use "(help sym)")
  (info "Display help information about #sym (unquoted).")
  (type macro)
  (arity 1)
  (result nil)
  (see (defhelp help-entry *help* apropos)))

(defhelp *help*
    (use "*help*")
  (info "Dict containing all help information for symbols.")
  (type dict)
  (arity 0)
  (result nil)
  (see (help defhelp apropos)))

(defun apropos (arg)
  (let ((info (get *help* arg nil)))
    (if info
	(cadr (assoc 'see info))
	nil)))

(defhelp apropos
    (use "(apropos sym) => #li")
  (info "Get a list of procedures and symbols related to #sym from the help system.")
  (type proc)
  (arity 1)
  (result list)
  (see (defhelp help-entry help *help*)))

(expect-ok (list? (list-foreach (dump) (lambda (x) (apropos x)))))

(defun _help-type-to-str (sym)
  (case sym
    ((proc func function) "procedure")
    ((special) "special form")
    ((macro) "macro")
    ((dict) "dict")
    ((string str) "string")
    ((array arr) "array")
    ((list) "list")
    ((a-list) "association list")
    ((color) "color list (r g b a)")
    ((color-spec) "color name or color list (r g b) or (r g b a)")
    (t (fmt "%v" sym))))

(defun _help-is-functional (sym)
  (member sym '(proc func function macro)))

(defun _help-arity-to-str (n)
  (cond
    ((< n 0) (fmt "%v or more" (abs (+ n 1))))
    (t (fmt "%v" n))))

(defun _help-see-to-str (li)
  (if li
      (apply str+
	     (cons (fmt "%v" (car li))
		   (mapcar
		    (cdr li)
		    (lambda (x) (fmt ", %v" x)))))
      "n/a"))

(defun _out-help (sym info)
  (if info
      (_out-help1 sym info)
      (outy
       (list
	(list 'fg 'z3s5-help-text
	      (list 'bg 'z3s5-help-back
		    (fmt "No help available for '%v.\n" sym)))))))

(defun _out-help1 (sym info)
  (outy
   `((fg z3s5-help-entry-text
	 (bg z3s5-help-entry-back
	     ,(if (_help-is-functional (cadr (assoc 'type info)))
		  (fmt "%v : %v/%v\n"
		       (_help-use info)
		       (_help-type-to-str (cadr (assoc 'type info)))
		       (_help-arity-to-str (cadr (assoc 'arity info))))
		  (fmt "%v : %v\n"
		       (_help-use info)
		       (_help-type-to-str (cadr (assoc 'type info)))))))
     ,@(_out-help-fmt-info (cadr (assoc 'info info)))
     (fg z3s5-help-text
	 (bg z3s5-help-back
	     ,(fmt " See also: %v." (_help-see-to-str (cadr (assoc 'see info))))))
     ,(_out-help-warn-maybe (cadr (assoc 'warn info)))
     (text "\n"))))

(defun _help-use (info)
  (cadr (assoc 'use info)))

(defun _out-help-warn-maybe (msg)
  (if (str? msg)
      (list 'fg (the-color 'z3s5-error-text)
	    (list 'bg (the-color 'z3s5-error-back)
		  (str+ " WARNING: " (strcase msg 'upper))))
      nil))

;; move last delimiter '(...(t "strD")(nil "other")...) => '(...(t "str") (nil "Dother")...)
;; TODO: this does not convert the borderline case when the last element is (t "strD").
(defun _out-help-fix-end (li)
  (letrec ((arr (list->array li))
	   (m (sub1 (len arr))))
    (dotimes (n m (array->list arr))
      (when (1st (array-ref arr n))
	(let ((a (array-ref arr n))
	      (b (array-ref arr (add1 n))))
	  (array-set arr (add1 n) (list (1st b) (str+ (slice (2nd a) (sub1 (len (2nd a))) (len (2nd a)))
						      (2nd b))))
	  (array-set arr n (list (1st a) (slice (2nd a) 0 (sub1 (len (2nd a)))))))))))


;; str-segment puts the delimiters in the segments marked with true,
;; we remove the # delimiter and move the space or ponctuation to the next
;; string using _out-help-fix-end
(defun _out-help-segment (s start end)
  (_out-help-fix-end
   (mapcar
    (str-segment s start end)
    (lambda (x)
      (if (1st x nil)
	  (list t (slice (2nd x) 1 (len (2nd x))))
	  x)))))

(defun _out-help-fmt-info (s)
  (mapcar
   (_out-help-segment s "#" " .,:()[]{}")
   (lambda (x)
     (if (1st x nil)
	 (list 'fg 'z3s5-help-entry-arg (list 'bg 'z3s5-help-back (2nd x)))
	 (list 'fg 'z3s5-help-text (list 'bg 'z3s5-help-back (2nd x)))))))


;; help for early defined macros
(defhelp case
    (use "(case expr (clause1 ... clausen)) => any")
  (info "Standard case macro, where you should use t for the remaining alternative. Example: (case (get dict 'key) ((a b) (out \"a or b\"))(t (out \"something else!\"))).")
  (type macro)
  (arity -3)
  (see (cond)))

(expect-true (case 'test
	       ((foo test) t)
	       ((bar testing) nil)))

(expect-true (case 'test
	       ((test) t)))

(expect-false (case 'test
		((foo bar) t)
		(t nil)))

(defhelp bound?
    (use "(bound? sym) => bool")
  (info "Return true if a value is bound to the symbol #sym, nil otherwise.")
  (type macro)
  (arity 1)
  (see (bind setq)))

(expect-true (bound? *tests*))
(expect-false (bound? this-is-not-bound))

(defhelp proc?
    (use "(proc? arg) => bool")
  (info "Return true if #arg is a procedure, nil otherwise.")
  (type macro)
  (arity 1)
  (see (functional? closure?  functional-arity functional-has-rest?)))

(expect-true (proc? (lambda (x) (out x))))
(expect-false (proc? expect))
(expect-false (proc? 1))
(expect-false (proc? (future (void))))
(expect-true (proc? void))

(defhelp functional?
    (use "(functional? arg) => bool")
  (info "Return true if #arg is either a builtin function, a closure, or a macro, nil otherwise. This is the right predicate for testing whether the argument is applicable and has an arity.")
  (type macro)
  (arity 1)
  (see (closure? proc? functional-arity functional-has-rest?)))

(expect-true (functional? (lambda (x) (out x))))
(expect-true (functional? expect))
(expect-true (functional? car))

;; SEQUENCE FUNCTIONS
(testing "sequences")

(defun nth (seq n)
  (cond
    ((array? seq) (array-ref seq n))
    ((list? seq) (list-ref seq n))
    ((str? seq) (str-ref seq n))
    (t (error "ref - not a sequence: %v" seq))))

(defhelp nth
    (use "(nth seq n) => any")
  (info "Get the #n-th element of sequence #seq. Sequences are 0-indexed.")
  (type proc)
  (arity 2)
  (see (nthdef list array string 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th)))

(expect 1 (nth #(2 1 3 0) 1))
(expect 9 (nth '(9 1 2 3 4) 0))
(expect 4 (nth '(9 1 2 3 4) 4))
(expect-err (nth "abc" 4))

(defun reverse (seq)
  (cond
    ((array? seq) (array-reverse seq))
    ((list? seq) (list-reverse seq))
    ((str? seq) (str-reverse seq))
    (t (error "reverse - not a sequence: %v" seq))))

(defhelp reverse
    (use "(reverse seq) => sequence")
  (info "Reverse a sequence non-destructively, i.e., return a copy of the reversed sequence.")
  (type proc)
  (arity 1)
  (see (nth seq? 1st 2nd 3rd 4th 6th 7th 8th 9th 10th last)))

(expect '(1 2 3 4 5 6) (reverse '(6 5 4 3 2 1)))
(expect #(1 2 3 4 5 6) (reverse #(6 5 4 3 2 1)))
(expect "123456" (reverse "654321"))
(expect-err (reverse (dict '(1 a 2 b 3 c 4 d 5 e 6 f))))

(defun nthdef (seq n default)
  (cond
    ((seq? seq) (if (or (< n 0) (>= n (len seq))) default (nth seq n)))
    (t default)))

(defhelp nthdef
    (use "(nthdef seq n default) => any")
  (info "Return the #n-th element of sequence #seq (0-indexed) if #seq is a sequence and has at least #n+1 elements, default otherwise.")
  (type proc)
  (arity 3)
  (see (nth seq? 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th)))

(expect 1 (nthdef '(9 8 7 6 1 5 4 3 2) 4 0))
(expect 9 (nthdef '(1 2 3 4 5) 6 9))
(expect 9 (nthdef nil 3 9))
(expect 4 (nthdef (dict) 3 4))

(defun _index (seq elem idx n pred)
  (cond
    ((= idx n) -1)
    ((pred (nth seq idx) elem) idx)
    (t (_index seq elem (add1 idx) n pred))))

(defun index (seq elem &rest pred)
  (_index seq elem 0 (len seq) (if pred (car pred) eq?)))

(defhelp index
    (use "(index seq elem [pred]) => int")
  (info "Return the first index of #elem in #seq going from left to right, using equality predicate #pred for comparisons (default is eq?). If #elem is not in #seq, -1 is returned.")
  (type proc)
  (arity -3)
  (see (nth seq?)))

(expect 3 (index '(2 3 4 1 9 9 7 8) 1))
(expect 4 (index '(a b c d e f g h i j k) 'e equal?))

(defun seq? (seq)
  (or (array? seq) (list? seq) (str? seq)))

(defhelp seq?
    (use "(seq? seq) => bool")
  (info "Return true if #seq is a sequence, nil otherwise.")
  (type proc)
  (arity 1)
  (see (list array string slice nth)))

(expect t (seq? '(a b c)))
(expect t (seq? #(a b c)))
(expect t (seq? "abc"))
(expect nil (seq? 1))
(expect nil (seq? (dict)))
(expect nil (seq? 'atom))

(defun slice (seq low high)
  (cond
    ((array? seq) (array-slice seq low high))
    ((list? seq) (list-slice seq low high))
    ((str? seq) (str-slice seq low high))
    (t (error "ref - not a sequence: %v" seq))))

(defhelp slice
    (use "(slice seq low high) => seq")
  (info "Return the subsequence of #seq starting from #low inclusive and ending at #high exclusive. Sequences are 0-indexed.")
  (type proc)
  (arity 3)
  (see (list array string nth seq?)))

(expect '(2 3 4) (slice '(1 2 3 4 5 6) 1 4))
(expect '(1 2 3 4 5 6) (slice '(1 2 3 4 5 6) 0 (len '(1 2 3 4 5 6))))
(expect "bcd" (slice "abcdefgbcd" 1 4))
(expect nil (slice '(1 2 3 4 5 6) 2 2))
(expect-err (slice '(1 2 3) 4 5))
(expect-err (slice nil 0 1))

(defun take (seq n)
  (slice seq 0 n))

(defhelp take
    (use "(take seq n) => seq")
  (info "Return the sequence consisting of the #n first elements of #seq.")
  (type proc)
  (arity 3)
  (see (list array string nth seq?)))

(expect '(1 2 3) (take '(1 2 3 4 5 6) 3))
(expect nil (take '(1 2 3 4 5 6) 0))

(defun map (seq proc)
  (cond
    ((list? seq) (mapcar seq proc))
    ((array? seq) (let ((c (array-copy seq)))
		    (dotimes (i (array-len seq))
		      (array-set c i (proc (array-ref seq i))))
		    c))
    ((str? seq) (strmap seq proc))
    (t (error "map - not a sequence: %v" seq))))

(defhelp map
    (use "(map seq proc) => seq")
  (info "Return the copy of #seq that is the result of applying #proc to each element of #seq.")
  (type proc)
  (arity 2)
  (see (seq? mapcar strmap)))

(expect '(2 3 4 5 6) (map '(1 2 3 4 5) add1))
(expect "bcdefg" (map "abcdef" (lambda (x) (+ x 1))))
(expect #(2 3 4 5 6 7) (map #(1 2 3 4 5 6) add1))
(expect-err (map (dict) add1))

(defun map-pairwise (seq proc)
  (unless (even? (len seq))
    (error "map-pairwise: expected sequence of even length, but the given sequence has length %v" (len seq)))
  (letrec ((k (len seq))
	   (traverse
	    (lambda (i s acc)
	      (cond
		((= i k) acc)
		((odd? i)
		 (traverse (add1 i) s (append acc (proc (nth s (sub1 i))(nth s i)))))
		(t
		 (traverse (add1 i) s acc)))))
	   (li (traverse 0 seq nil)))
    (cond
      ((list? seq) li)
      ((array? seq) (list->array li))
      ((str? seq) (list->str li))
      (t (error "map-pairwise - not a sequence: %v" seq)))))
		    
(defhelp map-pairwise
    (use "(map-pairwise seq proc) => seq")
  (info "Applies #proc in order to subsequent pairs in #seq, assembling the sequence that results from the results of #proc. Function #proc takes two arguments and must return a proper list containing two elements. If the number of elements in #seq is odd, an error is raised.")
  (type proc)
  (arity 2)
  (see (map)))

(expect '(1 2 3 4 5 6 7 8) (map-pairwise '(2 1 4 3 6 5 8 7) (lambda (x y) (list y x))))
(expect '(-1 3 -1 7 -1 11 -1 15) (map-pairwise '(1 2 3 4 5 6 7 8) (lambda (x y) (list (- x y) (+ x y)))))
(expect "abcdefgh" (map-pairwise "badcfehg" (lambda (x y) (list y x))))
(expect #(john smith peter wilkes maria calvesh) (map-pairwise #(smith john wilkes peter calvesh maria) (lambda (x y) (list y x))))
(expect-err (map-pairwise '(1 2 3) (lambda (x y) (list y x))))
(expect nil (map-pairwise '() (lambda (x y) (list x y))))
(expect '(1 2 3 4 5 6 7 8) (map-pairwise '(1 2 3 4 5 6 7 8) (lambda (x y) (list x y))))
(expect #(1 2 3 4 5 6 7 8) (map-pairwise #(1 2 3 4 5 6 7 8) (lambda (x y) (list x y))))
(expect "abcdefgh" (map-pairwise "abcdefgh" (lambda (x y) (list x y))))

(defun foreach (seq proc)
  (cond
    ((list? seq) (list-foreach seq proc))
    ((array? seq) (array-foreach seq proc))
    ((str? seq) (str-foreach seq proc))
    (t (error "foreach - not a sequence: %v" seq))))

(defhelp foreach
    (use "(foreach seq proc)")
  (info "Apply #proc to each element of sequence #seq in order, for the side effects.")
  (type proc)
  (arity 2)
  (see (seq? map)))

(expect 21 (let ((s 0))
	     (foreach '(1 2 3 4 5 6) (lambda (x) (setq s (+ s x))))
	     s))
(expect 21 (let ((s 0))
	     (foreach #(1 2 3 4 5 6) (lambda (x) (setq s (+ s x))))
	     s))
(expect 0 (let ((s 0))
	    (foreach nil (lambda (x) (setq s (+ s x))))
	    s))
(expect 700 (let ((s 0))
	      (foreach "abcdefg" (lambda (x) (setq s (+ s x))))
	      s))

(defun forall? (seq pred)
  (cond
    ((list? seq) (list-forall? seq pred))
    ((array? seq) (array-forall? seq pred))
    ((str? seq) (str-forall? seq pred))
    (t (error "all - not a sequence: %v" seq))))

(defhelp forall?
    (use "(forall? seq pred) => bool")
  (info "Return true if predicate #pred returns true for all elements of sequence #seq, nil otherwise.")
  (type proc)
  (arity 2)
  (see (foreach map list-forall? array-forall? str-forall? exists? str-exists? array-exists? list-exists?)))

(expect t (forall? '(1 2 3 4 5 6 7 8) num?))
(expect nil (forall? '(1 2 3 4 hello 5 6 7 8) num?))
(expect t (forall? "abcdefg" num?))
(expect t (forall? #(1.2 3.3 4.4 5.3 1.0) (lambda (x) (>= x 1))))
(expect nil (forall? #(0.2 1.2 3.3 -3.0 1.0) (lambda (x) (>= x 0))))

(defun list-forall? (li pred)
  (cond
    ((null? li) t)
    ((pred (car li)) (list-forall? (cdr li) pred))
    (t nil)))

(defhelp list-forall?
    (use "(list-all? li pred) => bool")
  (info "Return true if predicate #pred returns true for all elements of list #li, nil otherwise.")
  (type proc)
  (arity 2)
  (see (foreach map forall? array-forall? str-forall? exists?)))

(expect t (list-forall? '(1 2 3 4 5 6 7 8) num?))
(expect nil (list-forall? '(1 2 3 4 hello 5 6 7 8) num?))

(defun _seq-forall? (seq pred n m)
  (cond
    ((< n m)
     (cond
       ((pred (nth seq n))
	(_seq-forall? seq pred (add1 n) m))
       (t nil)))
    (t t)))

(defun array-forall? (arr pred)
  (_seq-forall? arr pred 0 (array-len arr)))

(defhelp array-forall?
    (use "(array-forall? arr pred) => bool")
  (info "Return true if predicate #pred returns true for all elements of array #arr, nil otherwise.")
  (type proc)
  (arity 2)
  (see (foreach map forall? str-forall? list-forall? exists?)))

(expect t (array-forall? #(1.2 3.3 4.4 5.3 1.0) (lambda (x) (>= x 1))))
(expect nil (array-forall? #(0.2 1.2 3.3 -3.0 1.0) (lambda (x) (>= x 0))))

(defun str-forall? (s pred)
  (let ((c (str->chars s)))
    (_seq-forall? c pred 0 (array-len c))))

(defhelp str-forall?
    (use "(str-forall? s pred) => bool")
  (info "Return true if predicate #pred returns true for all characters in string #s, nil otherwise.")
  (type proc)
  (arity 2)
  (see (foreach map forall? array-forall? list-forall exists?)))

(expect t (str-forall? "abcdefg" num?))

(defun exists? (seq pred)
  (cond
    ((list? seq) (list-exists? seq pred))
    ((array? seq) (array-exists? seq pred))
    ((str? seq) (str-exists? seq pred))
    (t
     (error "exists? - not a sequence: %v" seq))))

(defun list-exists? (li pred)
  (cond
    ((null? li) nil)
    ((pred (car li)) t)
    (t (list-exists? (cdr li) pred))))

(defun _seq-exists? (seq pred n m)
  (cond
    ((< n m)
     (cond
       ((pred (nth seq n)) t)
       (t (_seq-exists? seq pred (add1 n) m))))
    (t nil)))

(defun array-exists? (seq pred)
  (_seq-exists? seq pred 0 (array-len seq)))

(defun str-exists? (seq pred)
  (_seq-exists? seq pred 0 (strlen seq)))

(defhelp exists?
    (use "(exists? seq pred) => bool")
  (info "Return true if #pred returns true for at least one element in sequence #seq, nil otherwise.")
  (type proc)
  (arity 2)
  (see (forall? list-exists? array-exists? str-exists? seq?)))

(defhelp list-exists?
    (use "(list-exists? li pred) => bool")
  (info "Return true if #pred returns true for at least one element in list #li, nil otherwise.")
  (type proc)
  (arity 2)
  (see (exists? forall? array-exists? str-exists? seq?)))

(defhelp array-exists?
    (use "(array-exists? arr pred) => bool")
  (info "Return true if #pred returns true for at least one element in array #arr, nil otherwise.")
  (type proc)
  (arity 2)
  (see (exists? forall? list-exists? str-exists? seq?)))

(defhelp str-exists?
    (use "(str-exists? s pred) => bool")
  (info "Return true if #pred returns true for at least one character in string #s, nil otherwise.")
  (type proc)
  (arity 2)
  (see (exists? forall? list-exists? array-exists? seq?)))

(defun list-foreach (li proc)
  (cond
    (li
     (proc (car li))
     (list-foreach (cdr li) proc))
    (t (void))))

(defhelp list-foreach
    (use "(list-foreach li proc)")
  (info "Apply #proc to each element of list #li in order, for the side effects.")
  (type proc)
  (arity 2)
  (see (mapcar map foreach)))

(defun array-foreach (arr proc)
  (dotimes (i (array-len arr))
    (proc (array-ref arr i)))
  (void))

(defhelp array-foreach
    (use "(array-foreach arr proc)")
  (info "Apply #proc to each element of array #arr in order, for the side effects.")
  (type proc)
  (arity 2)
  (see (foreach list-foreach map)))

(defun str-foreach (s proc)
  (dotimes (i (len s))
    (proc (str-ref s i)))
  (void))

(defhelp str-foreach
    (use "(str-foreach s proc)")
  (info "Apply #proc to each element of string #s in order, for the side effects.")
  (type proc)
  (arity 2)
  (see (foreach list-foreach array-foreach map)))

(defun list->array (li)
  (apply array li))

(defhelp list->array
    (use "(list->array li) => array")
  (info "Convert the list #li to an array.")
  (type proc)
  (arity 1)
  (see (list array string nth seq?)))

(defun _remove-duplicates (li acc d)
  (cond
    ((null? li) (reverse acc))
    ((has-key? d (1st li nil)) (_remove-duplicates (cdr li) acc d))
    (t (set d (1st li nil) t)
       (_remove-duplicates (cdr li) (cons (car li) acc) d))))

(defun remove-duplicates (seq)
  (cond
    ((list? seq) (_remove-duplicates seq nil (dict)))
    ((array? seq) (list->array (_remove-duplicates (array->list seq) nil (dict))))
    ((str? seq) (list->str (_remove-duplicates (str->list seq) nil (dict))))
    (t (error "remove-duplicates: not a sequence: %v" seq))))

(defhelp remove-duplicates
    (use "(remove-duplicates seq) => seq")
  (info "Remove all duplicates in sequence #seq, return a new sequence with the duplicates removed.")
  (type proc)
  (arity 1)
  (see (seq? map foreach nth)))

(defun dict-map (d proc)
  (let ((cp (dict-copy d)))
    (dict-map! cp proc)
    cp))

(defhelp dict-map
    (use "(dict-map dict proc) => dict")
  (info "Returns a copy of #dict with #proc applies to each key value pair as aruments. Keys are immutable, so #proc must take two arguments and return the new value.")
  (type proc)
  (arity 2)
  (see (dict-map! map)))

(defun dict->alist (d)
  (let ((arr (build-array (len d) nil))
	(c 0))
    (dict-foreach
     d
     (lambda (k v)
       (array-set arr c (cons k v))
       (setq c (add1 c))))
    (array->list arr)))

(defhelp dict->alist
    (use "(dict->alist d) => li")
  (info "Convert a dictionary into an association list. Note that the resulting alist will be a set of proper pairs of the form '(a . b) if the values in the dictionary are not lists.")
  (type proc)
  (arity 1)
  (see (dict dict-map dict->list)))

(defun alist->dict (li)
  (let ((d (dict)))
    (foreach
     li
     (lambda (p)
       (set d (car p) (cdr p))))
    d))

(defhelp alist->dict (li)
  (use "(alist->dict li) => dict")
  (info "Convert an association list #li into a dictionary. Note that the value will be the cdr of each list element, not the second element, so you need to use an alist with proper pairs '(a . b) if you want b to be a single value.")
  (type proc)
  (arity 1)
  (see (dict->alist dict dict->list list->dict)))

(defun dict->keys (d)
  (let ((li nil))
    (dict-foreach
     d
     (lambda (k v)
       (setq li (cons k li))))
    li))

(defhelp dict->keys
    (use "(dict->keys d) => li")
  (info "Return the keys of dictionary #d in arbitrary order.")
  (type proc)
  (arity 1)
  (see (dict dict->values dict->alist dict->list)))

(defun dict->values (d)
  (let ((li nil))
    (dict-foreach
     d
     (lambda (k v)
       (setq li (cons v li))))
    li))

(defhelp dict->values
    (use "(dict->values d) => li")
  (info "Return the values of dictionary #d in arbitrary order.")
  (type proc)
  (arity 1)
  (see (dict dict->keys dict->alist dict->list)))

(defun get (d k &rest rargs)
  (_get d k (1st rargs nil)))

(defhelp get
    (use "(get dict key [default]) => any")
  (info "Get the value for #key in #dict, return #default if there is no value for #key. If #default is omitted, then nil is returned. Provide your own default if you want to store nil.")
  (type proc)
  (arity -3)
  (see (dict dict? set)))

(defun 1st (seq &rest default)
  (if (null? default)
      (nth seq 0)
      (nthdef seq 0 (car default))))

(defhelp 1st
    (use "(1st seq [default]) => any")
  (info "Get the first element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 2nd 3rd 4th 5th 6th 7th 8th 9th 10th)))

(defun 2nd (seq &rest default)
  (if (null? default)
      (nth seq 1)
      (nthdef seq 1 (car default))))

(defhelp 2nd
    (use "(2nd seq [default]) => any")
  (info "Get the second element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 3rd 4th 5th 6th 7th 8th 9th 10th)))

(defun 3rd (seq &rest default)
  (if (null? default)
      (nth seq 2)
      (nthdef seq 2 (car default))))

(defhelp 3rd
    (use "(3rd seq [default]) => any")
  (info "Get the third element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 4th 5th 6th 7th 8th 9th 10th)))

(defun 4th (seq &rest default)
  (if (null? default)
      (nth seq 3)
      (nthdef seq 3 (car default))))

(defhelp 4th
    (use "(4th seq [default]) => any")
  (info "Get the fourth element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 5th 6th 7th 8th 9th 10th)))

(defun 5th (seq &rest default)
  (if (null? default)
      (nth seq 4)
      (nthdef seq 4 (car default))))

(defhelp 5th
    (use "(5th seq [default]) => any")
  (info "Get the fifth element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 4th 6th 7th 8th 9th 10th)))

(defun 6th (seq &rest default)
  (if (null? default)
      (nth seq 5)
      (nthdef seq 5 (car default))))

(defhelp 6th
    (use "(6th seq [default]) => any")
  (info "Get the sixth element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 4th 5th 7th 8th 9th 10th)))

(defun 7th (seq &rest default)
  (if (null? default)
      (nth seq 6)
      (nthdef seq 6 (car default))))

(defhelp 7th
    (use "(7th seq [default]) => any")
  (info "Get the seventh element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 4th 5th 6th 8th 9th 10th)))

(defun 8th (seq &rest default)
  (if (null? default)
      (nth seq 7)
      (nthdef seq 7 (car default))))

(defhelp 8th
    (use "(8th seq [default]) => any")
  (info "Get the eighth element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 4th 5th 6th 7th 9th 10th)))

(defun 9th (seq &rest default)
  (if (null? default)
      (nth seq 8)
      (nthdef seq 8 (car default))))

(defhelp 9th
    (use "(9th seq [default]) => any")
  (info "Get the nineth element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 4th 5th 6th 7th 8th 10th)))

(defun 10th (seq &rest default)
  (if (null? default)
      (nth seq 9)
      (nthdef seq 9 (car default))))

(defhelp 10th
    (use "(10th seq [default]) => any")
  (info "Get the tenth element of a sequence or the optional #default. If there is no such element and no default is provided, then an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string-ref 1st 2nd 3rd 4th 5th 6th 7th 8th 9th)))

(defun last (seq &rest default)
  (if (null? default)
      (nth seq (sub1 (len seq)))
      (nthdef seq (sub1 (len seq))(car default))))

(defhelp last
    (use "(last seq [default]) => any")
  (info "Get the last element of sequence #seq or return #default if the sequence is empty. If #default is not given and the sequence is empty, an error is raised.")
  (type proc)
  (arity -2)
  (see (nth nthdef car list-ref array-ref string ref 1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th)))

;; list functions
(defun filter (li pred)
  (letrec ((filter-aux
	    (lambda (li acc)
	      (cond
		((not li) (nreverse acc))
		((pred (car li)) (filter-aux (cdr li) (cons (car li) acc)))
		(t (filter-aux (cdr li) acc))))))
    (filter-aux li nil)))

(defhelp filter
    (use "(filter li pred) => li")
  (info "Return the list based on #li with each element removed for which #pred returns nil.")
  (type proc)
  (arity 2)
  (see (list)))

;; check whether k is in dict d
(defun has (d k)
  (let ((s (gensym)))
    (if (equal? (get d k s) s) nil t)))

(defhelp has
    (use "(has dict key) => bool")
  (info "Return true if the dict #dict contains an entry for #key, nil otherwise.")
  (type proc)
  (arity 2)
  (see (dict get set)))

;; SYNCHRONIZATION PRIMITIVES
(defmacro with-mutex-lock (mu &rest body)
  `(progn
     (mutex-lock ,mu)
     (with-final (lambda (err result)
		   (mutex-unlock ,mu)
		   (if err
		       (*error-handler* err)
		       result))
       ,@body)))

(defhelp with-mutex-lock
    (use "(with-mutex-lock m ...) => any")
  (info "Execute the body with mutex #m locked for writing and unlock the mutex afterwards.")
  (type macro)
  (arity -2)
  (see (with-mutex-rlock make-mutex mutex-lock mutex-rlock mutex-unlock mutex-runlock))
  (warn "Make sure to never lock the same mutex twice from the same task, otherwise a deadlock will occur!"))

(defmacro with-mutex-rlock (mu &rest body)
   `(progn
     (mutex-rlock ,mu)
     (with-final (lambda (err result)
		   (mutex-runlock ,mu)
		   (if err
		       (*error-handler* err)
		       result))
       ,@body)))

(defhelp with-mutex-rlock
    (use "(with-mutex-rlock m ...) => any")
  (info "Execute the body with mutex #m locked for reading and unlock the mutex afterwards.")
  (type macro)
  (arity -2)
  (see (with-mutex-lock make-mutex mutex-lock mutex-rlock mutex-unlock mutex-runlock)))

;; synchronized queue
(defun make-queue ()
  (array 'queue (make-mutex) nil))

(defhelp make-queue
    (use "(make-queue) => array")
  (info "Make a synchronized queue.")
  (type proc)
  (arity 0)
  (see (queue? enqueue! dequeue! glance queue-empty? queue-len))
  (warn "Never change the array of a synchronized data structure directly, or your warranty is void!"))

(defun queue? (sym)
  (cond
    ((not (array? sym)) nil)
    ((< (len sym) 2) nil)
    ((not (mutex? (array-ref sym 1))) nil)
    (t (equal? (array-ref sym 0) 'queue))))

(defhelp queue?
    (use "(queue? q) => bool")
  (info "Return true if #q is a queue, nil otherwise.")
  (type proc)
  (arity 1)
  (see (make-queue enqueue! dequeue glance queue-empty? queue-len)))

(defmacro enqueue! (sym elem)
  `(progn 
     (mutex-lock (array-ref ,sym 1))
     (array-set ,sym 2 (append (array-ref ,sym 2) (list ,elem)))
     (mutex-unlock (array-ref ,sym 1))))

(defhelp enqueue!
    (use "(enqueue! sym elem)")
  (info "Put #elem in queue #sym, where #sym is the unquoted name of a variable.")
  (type macro)
  (arity 2)
  (see (make-queue queue? dequeue! glance queue-empty? queue-len)))

(defmacro dequeue! (sym &rest default)
  `(progn
     (mutex-lock (array-ref ,sym 1))
     (cond
       ((null? (array-ref ,sym 2))
	(mutex-unlock (array-ref ,sym 1))
	(if ',default (car ',default) nil))
       (t (let ((result (car (array-ref ,sym 2))))
	    (array-set ,sym 2 (cdr (array-ref ,sym 2)))
	    (mutex-unlock (array-ref ,sym 1))
	    result)))))

(defhelp dequeue!
    (use "(dequeue! sym [def]) => any")
  (info "Get the next element from queue #sym, which must be the unquoted name of a variable, and return it. If a default #def is given, then this is returned if the queue is empty, otherwise nil is returned.")
  (type macro)
  (arity -2)
  (see (make-queue queue? enqueue! glance queue-empty? queue-len)))

(defun queue-empty? (q)
  (mutex-rlock (array-ref q 1))
  (let ((result (null? (array-ref q 2))))
    (mutex-runlock (array-ref q 1))
    result))

(defhelp queue-empty?
    (use "(queue-empty? q) => bool")
  (info "Return true if the queue #q is empty, nil otherwise.")
  (type proc)
  (arity 1)
  (see (make-queue queue? enqueue! dequeue! glance queue-len)))

(defun queue-len (q)
  (mutex-rlock (array-ref q 1))
  (let ((result (len (array-ref q 2))))
    (mutex-runlock (array-ref q 1))
    result))

(defhelp queue-len
    (use "(queue-len q) => int")
  (info "Return the length of the queue #q.")
  (type proc)
  (arity 1)
  (see (make-queue queue? enqueue! dequeue! glance queue-len))
  (warn "Be advised that this is of limited use in some concurrent contexts, since the length of the queue might have changed already once you've obtained it!"))

(defun glance (q &rest default)
  (mutex-rlock (array-ref q 1))
  (let ((result (1st (array-ref q 2) (if default (car default) nil))))
    (mutex-runlock (array-ref q 1))
    result))

(defhelp glance
    (use "(glance s [def]) => any")
  (info "Peek the next element in a stack or queue without changing the data structure. If default #def is provided, this is returned in case the stack or queue is empty; otherwise nil is returned.")
  (type proc)
  (arity 1)
  (see (make-queue make-stack queue? enqueue? dequeue? queue-len stack-len pop! push!)))

;; synchronized stack
(defun make-stack ()
  (array 'stack (make-mutex) nil))

(defhelp make-stack
    (use "(make-stack) => array")
  (info "Make a synchronized stack.")
  (type proc)
  (arity 0)
  (see (stack? push! pop! stack-empty? stack-len glance))
  (warn "Never change the array of a synchronized data structure directly, or your warranty is void!"))

(defun stack? (sym)
  (cond
    ((not (array? sym)) nil)
    ((< (len sym) 2) nil)
    ((not (mutex? (array-ref sym 1))) nil)
    (t (equal? (array-ref sym 0) 'stack))))

(defhelp stack?
    (use "(stack? q) => bool")
  (info "Return true if #q is a stack, nil otherwise.")
  (type proc)
  (arity 1)
  (see (make-stack push! pop! stack-empty? stack-len glance)))

(defmacro push! (sym elem)
  `(progn 
     (mutex-lock (array-ref ,sym 1))
     (array-set ,sym 2 (cons ,elem  (array-ref ,sym 2)))
     (mutex-unlock (array-ref ,sym 1))))

(defhelp push!
    (use "(push! sym elem)")
  (info "Put #elem in stack #sym, where #sym is the unquoted name of a variable.")
  (type macro)
  (arity 2)
  (see (make-stack stack? pop! stack-len stack-empty? glance)))

(defmacro pop! (sym &rest default)
  `(progn
     (mutex-lock (array-ref ,sym 1))
     (cond
       ((null? (array-ref ,sym 2))
	(mutex-unlock (array-ref ,sym 1))
	(if ',default (car ',default) nil))
       (t (let ((result (car (array-ref ,sym 2))))
	    (array-set ,sym 2 (cdr (array-ref ,sym 2)))
	    (mutex-unlock (array-ref ,sym 1))
	    result)))))

(defhelp pop!
    (use "(pop! sym [def]) => any")
  (info "Get the next element from stack #sym, which must be the unquoted name of a variable, and return it. If a default #def is given, then this is returned if the queue is empty, otherwise nil is returned.")
  (type macro)
  (arity -2)
  (see (make-stack stack? push! stack-len stack-empty? glance)))

(defun stack-empty? (q)
  (mutex-rlock (array-ref q 1))
  (let ((result (null? (array-ref q 2))))
    (mutex-runlock (array-ref q 1))
    result))

(defhelp stack-empty?
    (use "(queue-empty? s) => bool")
  (info "Return true if the stack #s is empty, nil otherwise.")
  (type proc)
  (arity 1)
  (see (make-stack stack? push! pop! stack-len glance)))

(defun stack-len (q)
  (mutex-rlock (array-ref q 1))
  (let ((result (len (array-ref q 2))))
    (mutex-runlock (array-ref q 1))
    result))

(defhelp stack-len
    (use "(stack-len s) => int")
  (info "Return the length of the stack #s.")
  (type proc)
  (arity 1)
  (see (make-queue queue? enqueue! dequeue! glance queue-len))
  (warn "Be advised that this is of limited use in some concurrent contexts, since the length of the queue might have changed already once you've obtained it!"))

;; busy-polling synchronized dictionary access

(setq *sync-wait-lower-bound* 1)
(setq *sync-wait-upper-bound* 2000)

(defun _wait-for* (dict key timeout first-time)
  (cond
    ((or (get dict key nil) (if (>= timeout 0) (>= (- (now-ns) first-time) timeout) nil)) (get dict key nil))
    (t (sleep-ns (rand 0 *sync-wait-lower-bound*  *sync-wait-upper-bound*))
       (_wait-for* dict key timeout first-time))))

(defun wait-for* (dict key timeout)
  (_wait-for* dict key timeout (now-ns)))

(defhelp wait-for*
    (use "(wait-for* dict key timeout)")
  (info "Blocks execution until the value for #key in #dict is not-nil or #timeout nanoseconds have passed, and returns that value or nil if waiting timed out. If #timeout is negative, then the function waits potentially indefinitely without any timeout. If a non-nil key is not found, the function sleeps at least *sync-wait-lower-bound* nanoseconds and up to *sync-wait-upper-bound* nanoseconds until it looks for the key again.")
  (type proc)
  (arity 3)
  (see (future force wait-for wait-until wait-until*))
  (warn  "This cannot be used for synchronization of multiple tasks due to potential race-conditions."))

(defun wait-for (dict key)
  (wait-for* dict key -1))

(defhelp wait-for
    (use "(wait-for dict key)")
  (info "Block execution until the value for #key in #dict is not-nil. This function may wait indefinitely if no other thread sets the value for #key to not-nil.")
  (type proc)
  (arity 2)
  (see (wait-for* future force wait-until wait-until*))
  (warn  "This cannot be used for synchronization of multiple tasks due to potential race-conditions."))

(defun _wait-for-empty* (dict key timeout first-time)
  (when (and (has-key? dict key) (if (>= timeout 0) (< (- (now-ns) first-time) timeout) t))
    (sleep-ns (rand 0 *sync-wait-lower-bound*  *sync-wait-upper-bound*))
    (_wait-for-empty* dict key timeout first-time)))

(defun wait-for-empty* (dict key timeout)
  (_wait-for-empty* dict key timeout (now-ns)))

(defhelp wait-for-empty*
    (use "(wait-for-empty* dict key timeout)")
  (info "Blocks execution until the #key is no longer present in #dict or #timeout nanoseconds have passed. If #timeout is negative, then the function waits potentially indefinitely without any timeout.")
  (type proc)
  (arity 3)
  (see (future force wait-for wait-until wait-until*))
  (warn  "This cannot be used for synchronization of multiple tasks due to potential race-conditions."))

(defun _wait-until* (dict key pred timeout first-time)
  (let ((value (get dict key nil)))
    (cond
      ((or (pred value) (if (>= timeout 0) (>= (- (now-ns) first-time) timeout) nil)) value)
      (t (sleep-ns (rand 0 *sync-wait-lower-bound*  *sync-wait-upper-bound*))
	 (_wait-until* dict key pred timeout first-time)))))

(defun wait-until* (dict key pred timeout)
  (_wait-until* dict key pred timeout (now-ns)))

(defhelp wait-until*
    (use "(wait-until* dict key pred timeout)")
  (info "Blocks execution until the unary predicate #pred returns true for the value at #key in #dict, or #timeout nanoseconds have passed, and returns the value or nil if waiting timed out. If #timeout is negative, then the function waits potentially indefinitely without any timeout. If a non-nil key is not found, the function sleeps at least *sync-wait-lower-bound* nanoseconds and up to *sync-wait-upper-bound* nanoseconds until it looks for the key again.")
  (type proc)
  (arity 4)
  (see (future force wait-for wait-until* wait-until))
  (warn  "This cannot be used for synchronization of multiple tasks due to potential race-conditions."))

(defun wait-until (dict key pred)
  (wait-until* dict key pred -1))

(defhelp wait-until
    (use "(wait-until dict key pred)")
  (info "Blocks execution until the unary predicate #pred returns true for the value at #key in #dict. This function may wait indefinitely if no other thread sets the value in such a way that #pred returns true when applied to it.")
  (type proc)
  (arity 2)
  (see (wait-for future force wait-until*))
  (warn  "This cannot be used for synchronization of multiple tasks due to potential race-conditions."))

;; tasks (heavyweight alternative to futures)

(when (bound? *reflect*)
  (setq *reflect* (cons 'tasks *reflect*)))

(setq *task-counter* 0)(declare-unprotected '*task-counter*)
(setq *tasks* (dict))
(setq *tasks-state* (dict))
(setq *tasks-to-msg* (dict))
(setq *blackboard* nil)(declare-unprotected '*blackboard*)
(setq *blackboard-mutex* (make-mutex))
(setq *running-tasks* 0)(declare-unprotected '*running-tasks*)

(defun _new-task-id ()
  (cinc! '*task-counter*))

(defun task (sel proc)
  (let ((i (_new-task-id))
	(tproc (lambda (id)
		 (with-final (lambda (err result)
			       (cond
				 ((eq? sel 'remove)
				  (task-remove id))
				 (t
				  (cond
				    (err
				     (task-broadcast 'error id err)
				     (set *tasks-state* id 'error)
				     (*error-printer* err))
				    (t
				     (cond
				       ((eq? sel 'auto)
					(task-remove id))
				       (t (task-broadcast 'finished id result)
					  (set *tasks-state* id 'finished)))))))
			       (cdec! '*running-tasks*)
			       result)
		   (set *tasks-state* id 'running)
		   (cinc! '*running-tasks*)
		   (proc id)))))
    (set *tasks-state* i 'new)
    (set *tasks* i tproc)
    i))

(defhelp task
    (use "(task sel proc) => int")
  (info "Create a new task for concurrently running #proc, a procedure that takes its own ID as argument. The #sel argument must be a symbol in '(auto manual remove). If #sel is 'remove, then the task is always removed from the task table after it has finished, even if an error has occurred. If sel is 'auto, then the task is removed from the task table if it ends without producing an error. If #sel is 'manual then the task is not removed from the task table, its state is either 'canceled, 'finished, or 'error, and it and must be removed manually with #task-remove or #prune-task-table. Broadcast messages are never removed. Tasks are more heavy-weight than futures and allow for message-passing.")
  (type proc)
  (arity 1)
  (see (task? task-run task-state task-broadcast task-send task-recv task-remove prune-task-table)))

(defun task? (id)
  (and *tasks* (if (get *tasks* id nil) t nil)))

(defhelp task?
    (use "(task? id) => bool")
  (info "Check whether the given #id is for a valid task, return true if it is valid, nil otherwise.")
  (type proc)
  (arity 1)
  (see (task task-run task-state task-broadcast  task-send task-recv)))

(defun task-run (id &rest args)
  (let ((silent? (and args (1st args nil))))
    (if (task? id)
	(if (eq? (get *tasks-state* id nil) 'running)
	    (unless silent?
	      (error "task-run: task %v is already running" id))
	    (systask ((get *tasks* id) id)))
	(unless silent?
	  (error "task-run: invalid task %v" id)))))

(defhelp task-run
    (use "(task-run id)")
  (info "Run task #id, which must have been previously created with task. Attempting to run a task that is already running results in an error unless #silent? is true. If silent? is true, the function does never produce an error.")
  (type proc)
  (arity 1)
  (see (task task? task-state  task-send task-recv task-broadcast-)))

(defun task-remove (id)
  (delete *tasks-to-msg* id)
  (delete *tasks* id)
  (delete *tasks-state* id))

(defhelp task-remove
    (use "(task-remove id)")
  (info "Remove task #id from the task table. The task can no longer be interacted with.")
  (type proc)
  (arity 1)
  (see (task task? task-state)))

(defun prune-task-table ()
  (dict-foreach *tasks-state*
		(lambda (k v)
		  (when
		      (or (eq? v 'finished)
			  (eq? v 'canceled)
			  (eq? v 'error))
		    (task-remove k)))))

(defhelp prune-task-table
    (use "(prune-task-table)")
  (info "Remove tasks that are finished from the task table. This includes tasks for which an error has occurred.")
  (type proc)
  (arity 0)
  (see (task-remove task task? task-run)))

(defun task-state (id)
  (if (task? id)
      (get *tasks-state* id 'undefined)
      (error "task-state: invalid task %v" id)))

(defhelp task-state (id)
  (use "(task-state id) => sym")
  (info "Return the state of the task, which is a symbol in '(finished error stopped new waiting running).")
  (type proc)
  (arity 1)
  (see (task task? task-run task-broadcast  task-recv task-send)))

(defun task-broadcast (sel id &rest msg)
  (mutex-lock *blackboard-mutex*)
  (setq *blackboard* (cons (list sel (cons id msg)) *blackboard*))
  (mutex-unlock *blackboard-mutex*))

(defhelp task-broadcast
    (use "(task-broadcast id msg)")
  (info "Send a message from task #id to the blackboard. Tasks automatically send the message 'finished to the blackboard when they are finished.")
  (type proc)
  (arity 2)
  (see (task task? task-run task-state  task-send task-recv)))

(defun task-send (id msg)
  (when (not (task? id))
    (error "task-send: invalid task %v" id))
  (set *tasks-to-msg* id (array-append (get *tasks-to-msg* id (array)) msg)))

(defhelp task-send
    (use "(task-send id msg)")
  (info "Send a message #msg to task #id. The task needs to cooperatively use task-recv to reply to the message. It is up to the receiving task what to do with the message once it has been received, or how often to check for new messages.")
  (type proc)
  (arity 2)
  (see (task-broadcast task-recv task task? task-run task-state )))

(defun task-recv (id)
  (cond
    ((not (task? id))
     (error "task-recv: invalid task %v" id))
    (t 
     (letrec ((arr (get *tasks-to-msg* id nil))
	      (msg? (and arr (array? arr) (>= (len arr) 1)))
	      (msg (if msg? (array-ref arr 0) nil)))
       (when msg? (set *tasks-to-msg* id (array-slice arr 1 (len arr))))
       msg))))

(defhelp task-recv
    (use "(task-recv id) => any")
  (info "Receive a message for task #id, or nil if there is no message. This is typically used by the task with #id itself to periodically check for new messages while doing other work. By convention, if a task receives the message 'end it ought to terminate at the next convenient occasion, whereas upon receiving 'cancel it ought to terminate in an expedited manner.")
  (type proc)
  (arity 1)
  (see (task-send task task? task-run task-state task-broadcast))
  (warn "Busy polling for new messages in a tight loop is inefficient and ought to be avoided."))

(setq *scheduled-tasks* (make-queue))(declare-unprotected '*scheduled-tasks*)
(setq *scheduler-sleep-interval* 800)(declare-unprotected '*scheduler-sleep-interval*)
(setq *scheduler* nil)(declare-unprotected '*scheduler*)

(defun _schedule-task-future ()
  (task
   'auto
   (lambda (id)
     (while (not (eq? (task-recv id) 'end))
       (cond
	 ((queue-empty? *scheduled-tasks*) (sleep *scheduler-sleep-interval*))
	 (t (when (< *running-tasks* (cpunum)) (task-run (dequeue! *scheduled-tasks*) t))))))))

(defun start-task-scheduler ()
  (when (not (task? *scheduler*))
    (setq *scheduler* (_schedule-task-future)))
  (task-run *scheduler*))

(defun stop-task-scheduler ()
  (when (task? *scheduler*) (task-send *scheduler* 'end)))

(defun init-tasking ()
  (start-task-scheduler))

(defun task-schedule (id)
  (unless (task? id)
    (error "task-schedule: not a valid task %v" id))
  (cond
    ((>= *running-tasks* (cpunum))
     (enqueue! *scheduled-tasks* id))
    (t (task-run id))))

(defhelp task-schedule
    (use "(task-schedule sel id)")
  (info "Schedule task #id for running, starting it as soon as other tasks have finished. The scheduler attempts to avoid running more than (cpunum) tasks at once.")
  (type proc)
  (arity 1)
  (see (task task-run)))

(defun cpunum ()
  (2nd (sys 'version)))

(defhelp cpunum
    (use "(cpunum)")
  (info "Return the number of cpu cores of this machine.")
  (type proc)
  (arity 0)
  (see (sys))
  (warn "This function also counts virtual cores on the emulator. The original Z3S5 machine did not have virtual cpu cores."))

(setq *second-ns* 1000000000)
(setq *minute-ns* 60000000000)
(setq *hour-ns* 3600000000000)
(setq *quarter-ns* (div *hour-ns* 4))
(setq *halfhour-ns* (div *hour-ns* 2))
(setq *day-ns* (* *hour-ns* 24))
(setq *week-ns* (* *day-ns* 7))

(defun run-at (date repeater proc)
  (letrec ((nextns (_run-at-date->ns date repeater))
	   (tid (task 'auto
		      (lambda (id)
			(letrec ((loop
				    (lambda (next)
				      (let ((msg (task-recv id)))
					(cond
					  ((or (equal? msg 'end) (equal? msg 'cancel))
					   (void))
					  (t
					   (cond
					     ((<= next (now-ns))
					      (proc)
					      (sleep (rand 0 890 1020))
					      (when repeater (loop (repeater id next))))
					     (t (sleep (rand 0 890 1020))
						(loop next)))))))))
			  (loop nextns))))))
    (task-run tid)
    tid))

(defun _run-at-date->ns (date repeater)
  (let ((t0 (now-ns)))
    (cond
      ((list? date) (last (2nd date)))
      ((equal? date 'now) t0)
      ((equal? date 'next-minute) (+ t0 *minute-ns*))
      ((equal? date 'skip) (if repeater (repeater -1 t0) t0))
      ((equal? date 'next-quarter) (+ t0 *quarter-ns*))
      ((equal? date 'next-halfhour) (+ t0 *halfhour-ns*))
      ((equal? date 'next-hour) (+ t0 *hour-ns*))
      ((equal? date 'in-2-hours) (+ t0 (* 2 *hours-ns*)))
      ((equal? date 'in-3-hours) (+ t0 (* 3 *hours-ns*)))

      ((equal? date 'tomorrow) (+ t0 *day-ns*))
      ((equal? date 'next-week) (+ t0 (* *day-ns* 7)))
      ((equal? date 'next-month) (letrec ((d (now))
					  (Y (1st (1st d)))
					  (M (2nd (1st d)))
					  (D (3rd (1st d)))
					  (h (1st (2nd d)))
					  (m (2nd (2nd d)))
					  (s (3rd (2nd d))))
				   (date->epoch-ns Y (add1 M) D h m s 0)))
      ((equal? date 'next-year) (letrec ((d (now))
					 (Y (1st (1st d)))
					 (M (2nd (1st d)))
					 (D (3rd (1st d)))
					 (h (1st (2nd d)))
					 (m (2nd (2nd d)))
					 (s (3rd (2nd d))))
				  (date->epoch-ns (add1 Y) M D h m s 0))))))

(defhelp run-at
    (use "(run-at date repeater proc) => int")
  (info "Run procedure #proc with no arguments as task periodically according to the specification in #spec and return the task ID for the periodic task. Herbey, #date is either a datetime specification or one of '(now skip next-minute next-quarter next-halfhour next-hour in-2-hours in-3-hours tomorrow next-week next-month next-year), and #repeater is nil or a procedure that takes a task ID and unix-epoch-nanoseconds and yields a new unix-epoch-nanoseconds value for the next time the procedure shall be run. While the other names are self-explanatory, the 'skip specification means that the task is not run immediately but rather that it is first run at (repeater -1 (now)). Timing resolution for the scheduler is about 1 minute. Consider using interrupts for periodic events with smaller time resolutions. The scheduler uses relative intervals and has 'drift'.")
  (type proc)
  (arity 2)
  (see (task task-send))
  (warn "Tasks scheduled by run-at are not persistent! They are only run until the system is shutdown."))

;; ERROR HANDLING

;; do something with colors and restore the original ones
(defun with-colors (f b proc)
  (let ((fg (color 'text))
	(bg (color 'back)))
    (try ((set-color 'text fg)
	  (set-color 'back bg))
	 (set-color 'text (the-color f))
	 (set-color 'back (the-color b))
	 (proc))))

(defhelp with-colors
    (use "(with-colors textcolor backcolor proc)")
  (info "Execute #proc for display side effects, where the default colors are set to #textcolor and #backcolor. These are color specifications like in the-color. After #proc has finished or if an error occurs, the default colors are restored to their original state.")
  (type proc)
  (arity 3)
  (see ( the-color color set-color with-final)))

;; the printer for errors, if set; otherwise the unstyled default printer is used
(setq *error-printer*
      (lambda (err)
	(with-colors
	    (the-color 'z3s5-error-text)
	  (the-color 'z3s5-error-back)
	  (lambda ()
	    (beep 'error)
	    (out (if (bound? *error-printer-size-limit*)
		     (shorten err *error-printer-size-limit*)
		     err))
	    (out "\n")))))

(defhelp *error-printer*
    (use "(*error-printer* err)")
  (info "The global printer procedure which takes an error and prints it.")
  (type proc)
  (arity 1)
  (see (error)))

(setq *last-error* nil)(declare-unprotected '*last-error*)

(defhelp *last-error*
    (use "*last-error* => str")
  (info "Contains the last error that has occurred.")
  (type sym)
  (arity 0)
  (see (*error-printer* *error-handler*))
  (warn "This may only be used for debugging! Do *not* use this for error handling, it will surely fail!"))

(setq *error-handler*
      (dict '(0 (lambda (err) (setq *last-error* err)(*error-printer* err)))))

(setq *finalizers*
      (dict))

(setq *current-error-continuation*
      (dict))

(defhelp *error-handler*
    (use "(*error-handler* err)")
  (info "The global error handler dict that contains procedures which take an error and handle it. If an entry is nil, the default handler is used, which outputs the error using *error-printer*. The dict contains handlers based on concurrent thread IDs and ought not be manipulated directly.")
  (type dict)
  (arity 0)
  (see (*error-printer*)))

(defun warn (msg &rest args)
  (with-colors
      (the-color 'z3s5-warn-text)
    (the-color 'z3s5-warn-back)
    (lambda ()
      (let ((m (if args (apply fmt (cons msg args)) msg)))
	(print (fmt "*** warning ***%v" m))
	(out "*** warning *** ")
	(out m)
	(nl)))))

(defhelp warn
    (use "(warn msg [args...])")
  (info "Output the warning message #msg in error colors. The optional #args are applied to the message as in fmt. The message should not end with a newline.")
  (type proc)
  (arity -2)
  (see (error)))

;; add datum to stack in dictionary at key (last in, first out)
(defun pushstacked (dic key datum)
  (set dic key (cons datum (get dic key nil))))

(defhelp pushstacked
    (use "(pushstacked dict key datum)")
  (info "Push #datum onto the stack maintained under #key in the #dict.")
  (type proc)
  (arity 3)
  (see (getstacked popstacked)))

;; get datum from stack in dictionary at key (last in, first out)
(defun getstacked (dic key default)
  (let ((li (get dic key nil)))
    (if (null? li)
	default
	(car li))))

(defhelp getstacked
    (use "(getstacked dict key default)")
  (info "Get the topmost element from the stack stored at #key in #dict. If the stack is empty or no stack is stored at key, then #default is returned.")
  (type proc)
  (arity 3)
  (see (pushstacked popstacked)))

;; pop datum from stack in dictionary at key
(defun popstacked (dic key default)
  (let ((li (get dic key default)))
    (cond
      ((equal? li default) default)
      ((null? li) (delete dic key) default)
      (t (if (null? (cdr li))
	     (delete dic key)
	     (set dic key (cdr li)))
	 (car li)))))

(defhelp popstacked
    (use "(popstacked dict key default)")
  (info "Get the topmost element from the stack stored at #key in #dict and remove it from the stack. If the stack is empty or no stack is stored at key, then #default is returned.")
  (type proc)
  (arity 3)
  (see (pushstacked getstacked)))

(defun push-error-handler (proc)
  (pushstacked *error-handler* (sys 'taskid) proc))

(defhelp push-error-handler
    (use "(push-error-handler proc)")
  (info "Push an error handler #proc on the error handler stack. For internal use only.")
  (type proc)
  (arity 1)
  (see (with-error-handler)))

(defun pop-error-handler ()
  (popstacked *error-handler* (sys 'taskid) nil))

(defhelp pop-error-handler
    (use "(pop-error-handler) => proc")
  (info "Remove the topmost error handler from the error handler stack and return it. For internal use only.")
  (type proc)
  (arity 0)
  (see (with-error-handler)))

(defun current-error-handler ()
  (let ((handler (getstacked *error-handler* (sys 'taskid) nil)))
    (if handler
	handler
	(getstacked *error-handler* 0 (default-error-handler)))))

(defhelp current-error-handler
    (use "(current-error-handler) => proc")
  (info "Return the current error handler, a default if there is none.")
  (type proc)
  (arity 0)
  (see (default-error-handler push-error-handler pop-error-handler *current-error-handler* *current-error-continuation*)))

(defun default-error-handler ()
  (lambda (err) (beep 'error)(*error-printer* err)))

(defhelp default-error-handler
    (use "(default-error-handler) => proc")
  (info "Return the default error handler, irrespectively of the current-error-handler.")
  (type proc)
  (arity 0)
  (see (current-error-handler push-error-handler pop-error-handler *current-error-handler* *current-error-continuation*)))

(defun push-finalizer (proc)
  (pushstacked *finalizers* (sys 'taskid) proc))

(defhelp push-finalizer
    (use "(push-finalizer proc)")
  (info "Push a finalizer procedure #proc on the finalizer stack. For internal use only.")
  (type proc)
  (arity 1)
  (see (with-final pop-finalizer)))

(defun pop-finalizer ()
  (popstacked *finalizers* (sys 'taskid)
	      (lambda (err result)
		(when err
		  (*error-handler* err))
		result)))

(defhelp pop-finalizer
    (use "(pop-finalizer) => proc")
  (info "Remove a finalizer from the finalizer stack and return it. For internal use only.")
  (type proc)
  (arity 0)
  (see (push-finalizer with-final)))

(defmacro with-error-handler (handler &rest body)
  (let ((result (gensym)))
    `(let ((,result nil))
       (push-error-handler ,handler)
       (setq ,result (progn ,@body))
       (void (pop-error-handler))
       ,result)))

(defhelp with-error-handler
    (use "(with-error-handler handler body ...)")
  (info "Evaluate the forms of the #body with error handler #handler in place. The handler is a procedure that takes the error as argument and handles it. If an error occurs in #handler, a default error handler is used. Handlers are only active within the same thread.")
  (type macro)
  (arity -3)
  (see (with-final)))

(defmacro with-final (finalizer &rest body)
  (let ((result (gensym)))
    `(let ((,result nil))
       (push-finalizer ,finalizer)
       (push-error-handler
	(lambda (err)
	  (void (pop-error-handler))
	  ((pop-finalizer) err ,result)
	  (let ((handler (pop-error-handler)))
	    (when handler (handler err)))))
       (setq ,result (progn ,@body))
       (pop-error-handler)
       ((pop-finalizer) nil ,result))))

(defhelp with-final
    (use "(with-final finalizer body ...)")
  (info "Evaluate the forms of the #body with the given finalizer as error handler. If an error occurs, then #finalizer is called with that error and nil. If no error occurs, #finalizer is called with nil as first argument and the result of evaluating all forms of #body as second argument.")
  (type macro)
  (arity -3)
  (see (with-error-handler)))

(defmacro try (final-stms &rest body)
  `(progn (push-error-handler
	   (lambda (err) (progn
			   (when err (pop-error-handler))
			   ,@final-stms
			   (when err (*error-printer* err)))))
	  ,@body
	  (pop-error-handler)
	  ,@final-stms))

(defhelp try
    (use "(try (finals ...) body ...)")
  (info "Evaluate the forms of the #body and afterwards the forms in #finals. If during the execution of #body an error occurs, first all #finals are executed and then the error is printed by the default error printer.")
  (type macro)
  (arity -3)
  (see (with-final with-error-handler)))

;; HOOKS
;; check whether the hook is supported

(setq *custom-hooks* (dict))
(dfc! '*custom-hook-counter*)

(setq *hooks*
      (dict))

(defhelp *hooks*
    (use "*hooks*")
  (info "A dict containing translations from symbolic names to the internal numeric representations of hooks.")
  (type dict)
  (arity 0)
  (see (hook add-hook remove-hook remove-hooks)))

(defun hook (h)
  (let ((id (get *hooks* h nil)))
    (if id
	id
	(get *custom-hooks* h nil))))

(defhelp hook
    (use "(hook symbol)")
  (info "Lookup the internal hook number from a symbolic name.")
  (type proc)
  (arity 1)
  (see (*hooks* add-hook remove-hook remove-hooks)))

;; add a hook if it is supported
(defun add-hook (h proc)
  (let ((id (hook h)))
    (if (num? id)
	(add-hook-internal id proc)
	nil)))

(defhelp add-hook
    (use "(add-hook hook proc) => id")
  (info "Add hook procedure #proc which takes a list of arguments as argument under symbolic or numeric #hook and return an integer hook #id for this hook. If #hook is not known, nil is returned.")
  (type proc)
  (arity 2)
  (see (remove-hook remove-hooks replace-hook)))

(setq *hooks-added-once* (dict))
(declare-unprotected *hooks-added-once*)

(defun add-hook-once (h proc)
  (cond
    ((get *hooks-added-once* h)
     (set *hooks-added-once* h true)
     (add-hook h proc))
    (t nil)))

(defhelp add-hook-once
    (use "(add-hook-once hook proc) => id")
  (info "Add a hook procedure #proc which takes a list of arguments under symbolic or numeric #hook and return an integer hook #id. If #hook is not known, nil is returned.")
  (type proc)
  (arity 2)
  (see (add-hook remove-hook replace-hook)))

(defun def-custom-hook (h)
  (cinc! '*custom-hook-counter*)
  (set *custom-hooks* h (+ 65635 *custom-hook-counter*)))

(defhelp def-custom-hook
    (use "(def-custom-hook sym proc)")
  (info "Define a custom hook point, to be called manually from Lisp. These have IDs starting from 65636.")
  (type proc)
  (arity 2)
  (see (add-hook)))

(defun run-hook (h &rest args)
  (let ((id (hook h)))
    (if (num? id)
	(apply run-hook-internal (cons id args))
	nil)))

(defhelp run-hook
    (use "(run-hook hook)")
  (info "Manually run the hook, executing all procedures for the hook.")
  (type proc)
  (arity 1)
  (see (add-hook remove-hook)))

;; remove a callback for a hook by id
(defun remove-hook (h id2)
  (let ((id1 (hook h)))
    (if (num? id1)
	(progn (remove-hook-internal id1 id2) t)
	nil)))

(defhelp remove-hook
    (use "(remove-hook hook id) => bool")
  (info "Remove the symbolic or numberic #hook with #id and return true if the hook was removed, nil otherwise.")
  (type proc)
  (arity 2)
  (see (add-hook remove-hooks replace-hook)))

;; remove all callbacks for the hook
(defun remove-hooks (h)
  (let ((id (hook h)))
    (if (num? id)
	(progn (remove-hooks-internal id) t)
	nil)))

(defhelp remove-hooks
    (use "(remove-hooks hook) => bool")
  (info "Remove all hooks for symbolic or numeric #hook, return true if the hook exists and the associated procedures were removed, nil otherwise.")
  (type proc)
  (arity 1)
  (see (add-hook remove-hook replace-hook)))

;; replace all hooks with the given one
(defun replace-hook (h proc)
  (remove-hooks h)
  (add-hook h proc))

(defhelp replace-hook
    (use "(replace-hook hook proc)")
  (info "Remove all hooks for symbolic or numeric #hook and install the given #proc as the only hook procedure.")
  (type proc)
  (arity 2)
  (see (add-hook remove-hook remove-hooks)))

;; MATH FUNCTIONS

(setq *e* 2.71828182845904523536028747135266249775724709369995957496696763)
(setq *pi* 3.14159265358979323846264338327950288419716939937510582097494459)
(setq *phi* 1.61803398874989484820458683436563811772030917980576286213544862)
(setq *sqrt2* 1.41421356237309504880168872420969807856967187537694807317667974)
(setq *sqrte* 1.64872127070012814684865078781416357165377610071014801157507931)
(setq *sqrtpi* 1.77245385090551602729816748334114518279754945612238712821380779)
(setq *sqrtphi* 1.27201964951406896425242246173749149171560804184009624861664038)
(setq *ln2* 0.693147180559945309417232121458176568075500134360255254120680009)
(setq *log2e* (/ 1 *ln2*))
(setq *ln10* 2.30258509299404568401799145468436420760110148862877297603332790)
(setq *log10e* (/ 1 *ln10*))
(setq *max-float32* 3.40282346638528859811704183484516925440e+38)
(setq *smallest-nonzero-float32* 1.401298464324817070923729583289916131280e-45)
(setq *max-float64* 1.797693134862315708145274237317043567981e+308)
(setq *smallest-nonzero-float64* 4.940656458412465441765687928682213723651e-324)

(defun even? (x)
  (= (mod (abs x) 2) 0))

(defhelp even?
    (use "(even? n) => bool")
  (info "Returns true if the integer #n is even, nil if it is not even.")
  (type proc)
  (arity 1)
  (see (odd?)))

(defun odd? (x)
  (not (even? x)))

(defhelp odd?
    (use "(odd? n) => bool")
  (info "Returns true if the integer #n is odd, nil otherwise.")
  (type proc)
  (arity 1)
  (see (even?)))

;; UTILITY FUNCTIONS


(defun shorten (s n)
  (cond
    ((not (str? s)) s)
    ((< (len s) n) s)
    (t
     (str+ (slice s 0 (div (- n 3) 2)) "..." (slice s (div (- n 3) 2) (- n 3))))))

(defhelp shorten
    (use "(shorten s n) => str")
  (info "Shorten string #s to length #n in a smart way if possible, leave it untouched if the length of #s is smaller than #n.")
  (type proc)
  (arity 2)
  (see (substr)))

(defun str->list (s)
  (array->list (str->array s)))

(defhelp str->list
    (use "(str->list s) => list")
  (info "Return the sequence of numeric chars that make up string #s.")
  (type proc)
  (arity 1)
  (see (str->array list->str array->str chars)))

(defun str-remove-number (s &rest opt)
  (let ((fields (array->list (strsplit s (if opt (1st opt) " ")))))
    (if (num? (str->expr (last fields)))
	(str-join (reverse (cdr (reverse fields))) (if opt (1st opt) " "))
	s)))

(defhelp str-remove-number
    (use "(str-remove-number s [del]) => str")
  (info "Remove the suffix number in #s, provided there is one and it is separated from the rest of the string by #del, where the default is a space character. For instance, \"Test 29\" will be converted to \"Test\", \"User-Name1-23-99\" with delimiter \"-\" will be converted to \"User-Name1-23\". This function will remove intermediate delimiters in the middle of the string, since it disassembles and reassembles the string, so be aware that this is not preserving inputs in that respect.")
  (type proc)
  (arity 1)
  (see (strsplit)))

(defun str-remove-prefix (s pfx)
  (cond
    ((or (str-empty? pfx) (str-empty? s)) s)
    ((equal? (str-slice s 0 (min (len pfx)(len s)))
	     pfx)
     (str-slice s (min (len pfx) (len s)) (len s)))
    (t s)))

(defhelp str-remove-prefix
    (use "(str-remove-prefix s prefix) => str")
  (info "Remove the prefix #prefix from string #s, return the string without the prefix. If the prefix does not match, #s is returned. If #prefix is longer than #s and matches, the empty string is returned.")
  (type proc)
  (arity 1)
  (see (str-remove-suffix)))

(defun str-remove-suffix (s sfx)
  (cond
    ((or (str-empty? sfx) (str-empty? s)) s)
    ((equal? (str-slice s (- (len s) (min (len sfx) (len s))) (len s))
	     sfx)
     (str-slice s (- (len s) (min (len sfx) (len s))) (len s)))
    (t s)))

(defhelp str-remove-suffix
    (use "(str-remove-suffix s suffix) => str")
  (info "remove the suffix #suffix from string #s, return the string without the suffix. If the suffix does not match, #s is returned. If #suffix is longer than #s and matches, the empty string is returned.")
  (type proc)
  (arity 1)
  (see (str-remove-prefix)))

(defun str-join (fields sep)
  (_str-join fields sep ""))

(defun _str-join (fields sep acc)
  (cond
    ((null? fields) (if (str-empty? acc) "" (str-slice acc 0 (- (strlen acc) (strlen sep)))))
    (t (_str-join (cdr fields) sep (str+ acc (car fields) sep)))))

(defhelp str-join
    (use "(str-join li del) => str")
  (info "Join a list of strings #li where each of the strings is separated by string #del, and return the result string.")
  (type proc)
  (arity 2)
  (see (strlen strsplit str-slice)))

(defun list->str (li)
  (array->str (list->array li)))

(defhelp list->str
    (use "(list->str li) => string")
  (info "Return the string that is composed out of the chars in list #li.")
  (type proc)
  (arity 1)
  (see (array->str str->list chars)))

(defun alist? (li)
  (if (null? li)
      t
      (and (list? li)
	   (list? (car li))
	   (alist? (cdr li)))))

(defhelp alist?
    (use "(alist? li) => bool")
  (info "Return true if #li is an association list, nil otherwise. This also works for a-lists where each element is a pair rather than a full list.")
  (type proc)
  (arity 1)
  (see (assoc)))

(defun assoc1 (sym li)
  (cadr (assoc sym li)))

(defhelp assoc1 
    (use "(assoc1 sym li) => any")
  (info "Get the second element in the first sublist in #li that starts with #sym. This is equivalent to (cadr (assoc sym li)).")
  (type proc)
  (arity 2)
  (see (assoc alist?)))

(defun make-set (&rest opt)
  (list->set opt))

(defhelp make-set
    (use "(make-set [arg1] ... [argn]) => dict")
  (info "Create a dictionary out of arguments #arg1 to #argn that stores true for very argument.")
  (type proc)
  (arity -1)
  (see (list->set set->list set-element? set-union set-intersection set-complement set-difference set? set-empty?)))

(defun list->set (li)
  (alist->dict (map li (lambda (x) (list x t)))))

(defhelp list->set
    (use "(list->set li) => dict")
  (info "Create a dict containing true for each element of list #li.")
  (type proc)
  (arity 1)
  (see (make-set set-element? set-union set-intersection set-complement set-difference set? set-empty)))

(defun set->list (s)
  (map (dict->alist s) (lambda (x) (car x))))

(defhelp set->list
    (use "(set->list s) => li")
  (info "Convert set #s to a list of set elements.")
  (type proc)
  (arity 1)
  (see (list->set make-set set-element? set-union set-intersection set-complement set-difference set? set-empty)))

(defun set? (x)
  (dict? x))

(defhelp set?
    (use "(set? x) => bool")
  (info "Return true if #x can be used as a set, nil otherwise.")
  (type proc)
  (arity 1)
  (see (list->set make-set set->list set-element? set-union set-intersection set-complement set-difference set-empty?)))

(defun set-element? (s elem)
  (has-key? s elem))

(defhelp set-element?
    (use "(set-element? s elem) => bool")
  (info "Return true if set #s has element #elem, nil otherwise.")
  (type proc)
  (arity 2)
  (see (make-set list->set set->list set-union set-intersection set-complement set-difference set? set-empty?)))

(defun set-empty? (s)
  (dict-empty? s))

(defhelp set-empty?
    (use "(set-empty? s) => bool")
  (info "Return true if set #s is empty, nil otherwise.")
  (type proc)
  (arity 1)
  (see (make-set list->set set->list set-union set-intersection set-complement set-difference set?)))

(defun dict-merge (a b)
  (let ((c (dict-copy a)))
    (dict-foreach b (lambda (k v) (unless (has-key? a k) (set c k v))))
    c))

(defhelp dict-merge
    (use "(dict-merge a b) => dict")
  (info "Create a new dict that contains all key-value pairs from dicts #a and #b. Note that this function is not symmetric. If a key is in both #a and #b, then the key value pair in #a is retained for this key.")
  (type proc)
  (arity 2)
  (see (dict dict-map dict-map! dict-foreach)))

(defun set-union (a b)
  (dict-merge a b))

(defhelp set-union
    (use "(set-union a b) => set")
  (info "Return the union of sets #a and #b containing all elements that are in #a or in #b (or both).")
  (type proc)
  (arity 2)
  (see (list->set set->list make-set set-element? set-intersection set-complement set-difference set? set-empty?)))

(defun set-intersection (a b)
  (let ((c (dict)))
    (dict-foreach a (lambda (k v) (when (has-key? b k) (set c k t))))
    c))

(defhelp set-intersection
    (use "(set-intersection a b) => set")
  (info "Return the intersection of sets #a and #b, i.e., the set of elements that are both in #a and in #b.")
  (type proc)
  (arity 2)
  (see (list->set set->list make-set set-element? set-union set-complement set-difference set? set-empty? set-subset? set-equal?)))

(defun set-difference (a b)
  (let ((c (dict)))
    (dict-foreach a (lambda (k v) (unless (has-key? b k) (set c k t))))
    c))

(defhelp set-difference
    (use "(set-difference a b) => set")
  (info "Return the set-theoretic difference of set #a minus set #b, i.e., all elements in #a that are not in #b.")
  (type proc)
  (arity 2)
  (see (list->set set->list make-set set-element? set-union set-intersection set-complement set? set-empty? set-subset? set-equal?)))

(defun set-complement (a b)
  (let ((c (dict)))
    (dict-foreach b (lambda (k v) (unless (has-key? a k) (set c k t))))
    c))

(defhelp set-complement
    (use "(set-complement a domain) => set")
  (info "Return all elements in #domain that are not elements of #a.")
  (type proc)
  (arity 2)
  (see (list->set set->list make-set set-element? set-union set-difference set-intersection set? set-empty? set-subset? set-equal?)))

(defun set-subset? (a b)
  (dict-key-subset? a b))

(defhelp set-subset?
    (use "(set-subset? a b) => bool")
  (info "Return true if #a is a subset of #b, nil otherwise.")
  (type proc)
  (arity 2)
  (see (set-equal? list->set set->list make-set set-element? set-union set-difference set-intersection set-complement set? set-empty?)))

(defun set-equal? (a b)
  (dict-key-equal? a b))

(defhelp set-equal?
    (use "(set-equal? a b) => bool")
  (info "Return true if #a and #b contain the same elements.")
  (type proc)
  (arity 2)
  (see (set-subset? list->set set-element? set->list set-union set-difference set-intersection set-complement set? set-empty?)))

(defun chars (s)
  (list->set (str->chars s)))

(defhelp chars
    (use "(chars str) => dict")
  (info "Return a charset based on #str, i.e., dict with the chars of #str as keys and true as value.")
  (type proc)
  (arity 1)
  (see (dict get set contains)))

(defun inchars (c chs)
  (set-element? chs c))

(defhelp inchars
    (use "(inchars char chars) => bool")
  (info "Return true if char is in the charset chars, nil otherwise.")
  (type proc)
  (arity 2)
  (see (chars dict get set has)))

(defun _str-index (s chars pos end)
  (cond
    ((>= pos end) nil)
    ((inchars (str->char (slice s pos (add1 pos))) chars)
     pos)
    (t (_str-index s chars (add1 pos) end))))

(defun str-index (s chars &rest pos)
  (_str-index s chars (if (null? pos) 0 (car pos)) (strlen s)))

(defhelp str-index
    (use "(str-index s chars [pos]) => int")
  (info "Find the first char in #s that is in the charset #chars, starting from the optional #pos in #s, and return its index in the string. If no macthing char is found, nil is returned.")
  (type proc)
  (arity -3)
  (see (strsplit chars inchars)))

(defun _str-segment (s sc ec acc)
  (let ((start (str-index s sc)))
    (if (not start)
	(append acc (list (list nil s)))
	(let ((end (str-index s ec (add1 start))))
	  (cond
	    ((or (not end) (<= end start))
	     (append acc
		     (list (list nil (slice s 0 start))
			   (list t (slice s start (len s))))))
	    (t (_str-segment
		(slice s (add1 end) (len s))
		sc ec
		(append acc (list
			     (list nil (slice s 0 start))
			     (list t (slice s start (add1 end))))))))))))

(defun str-segment (s start end)
  (filter (_str-segment s (chars start) (chars end) nil)
	  (lambda (x) (not (equal? (2nd x) "")))))

(defhelp str-segment
    (use "(str-segment str start end) => list")
  (info "Parse a string #str into words that start with one of the characters in string #start and end in one of the characters in string #end and return a list consisting of lists of the form (bool s) where bool is true if the string starts with a character in #start, nil otherwise, and #s is the extracted string including start and end characters.")
  (type proc)
  (arity 3)
  (see (str+ strsplit fmt strbuild)))

(defun darken (col &rest amount)
  (letrec ((c (the-color col))
	   (delta (_darken-delta c (if amount (car amount) 30))))
    (list (max (- (1st c) delta) 0)
	  (max (- (2nd c) delta) 0)
	  (max (- (3rd c) delta) 0)
	  (4th c))))

(defun _darken-delta (c proposed)
  (letrec ((li (reverse (cdr (reverse c))))
	   (m (apply min li)))
    (if (< m proposed)
	m
	proposed)))

(defhelp darken
    (use "(darken color [amount]) => (r g b a)")
  (info "Return a darker version of #color. The optional positive #amount specifies the amount of darkening (0-255).")
  (type proc)
  (arity 1)
  (see (the-color *colors* lighten)))

(defun lighten (col &rest amount)
  (letrec ((c (the-color col))
	   (delta (_lighten-delta c (if amount (car amount) 30))))
    (list (min (+ (1st c 180) delta) 255)
	  (min (+ (2nd c 180) delta) 255)
	  (min (+ (3rd c 180) delta) 255)
	  (4th c))))

(defun _lighten-delta (c proposed)
  (letrec ((li (reverse (cdr (reverse c))))
	   (m (apply max li)))
    (min proposed (- 255 m))))

(defhelp lighten
    (use "(lighten color [amount]) => (r g b a)")
  (info "Return a lighter version of #color. The optional positive #amount specifies the amount of lightening (0-255).")
  (type proc)
  (arity 1)
  (see (the-color *colors* darken)))

(defun random-color (&rest alpha)
  (list (rand 0 0 255) (rand 0 0 255) (rand 0 0 255) (1st alpha 255)))

(defhelp random-color
    (use "(random-color [alpha])")
  (info "Return a random color with optional #alpha value. If #alpha is not specified, it is 255.")
  (type proc)
  (arity -1)
  (see (the-color *colors* darken lighten)))

(defun minmax (pred li so-far)
  (cond
    ((null? li) so-far)
    ((pred (car li) so-far) (minmax pred (cdr li) (car li)))
    (t (minmax pred (cdr li) so-far))))

(defhelp minmax
    (use "(minmax pred li acc) => any")
  (info "Go through #li and test whether for each #elem the comparison (pred elem acc) is true. If so, #elem becomes #acc. Once all elements of the list have been compared, #acc is returned. This procedure can be used to implement generalized minimum or maximum procedures.")
  (type proc)
  (arity 3)
  (see (min max)))

(defun min (&rest args)
  (minmax < (cdr args) (car args)))

(defhelp min
    (use "(min x1 x2 ...) => num")
  (info "Return the minimum of the given numbers.")
  (type proc)
  (arity -2)
  (see (max minmax)))

(defun max (&rest args)
  (minmax > (cdr args) (car args)))

(defhelp max
    (use "(max x1 x2 ...) => num")
  (info "Return the maximum of the given numbers.")
  (type proc)
  (arity -2)
  (see (min minmax)))

(defun now-ms ()
  (div (now-ns) 1000000))

(defhelp now-ms
    (use "(now-ms) => num")
  (info "Return the relative system time as a call to (now-ns) but in milliseconds.")
  (type proc)
  (arity 0)
  (see (now-ns now)))

(defun datelist->epoch-ns (d)
  (let ((unix-ns (5th (2nd d nil) nil)))
    (if unix-ns
	unix-ns
	(date->epoch-ns (1st (1st d))
			(2nd (1st d) 1)
			(3rd (1st d) 1)
			(1st (2nd d nil) 12)
			(2nd (2nd d nil) 0)
			(3rd (2nd d nil) 0)
			(4th (2nd d nil) 0)))))

(defhelp datelist->epoch-ns
    (use "(datelist->epoch-ns dateli) => int")
  (info "Convert a datelist to Unix epoch nanoseconds. This function uses the Unix nanoseconds from the 5th value of the second list in the datelist, as it is provided by functions like (now). However, if the Unix nanoseconds value is not specified in the list, it uses #date->epoch-ns to convert to Unix epoch nanoseconds. Datelists can be incomplete. If the month is not specified, January is assumed. If the day is not specified, the 1st is assumed. If the hour is not specified, 12 is assumed, and corresponding defaults for minutes, seconds, and nanoseconds are 0.")
  (type proc)
  (arity 1)
  (see (date->epoch-ns datestr datestr* datestr->datelist epoch-ns->datelist now)))
  
(defun sec+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns ti) (* *second-ns* delta))))

(defhelp sec+
    (use "(sec+ dateli n) => dateli")
  (info "Adds #n seconds to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (minute+ hour+ day+ week+ month+ year+ now)))

(defun minute+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns ti) (* *minute-ns* delta))))

(defhelp minute+
    (use "(minute+ dateli n) => dateli")
  (info "Adds #n minutes to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (sec+ hour+ day+ week+ month+ year+ now)))

(defun hour+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns ti) (* *hour-ns* delta))))

(defhelp hour+
    (use "(hour+ dateli n) => dateli")
  (info "Adds #n hours to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (sec+ minute+ day+ week+ month+ year+ now)))

(defun day+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns ti) (* *day-ns* delta))))

(defhelp day+
    (use "(day+ dateli n) => dateli")
  (info "Adds #n days to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (sec+ minute+ hour+ week+ month+ year+ now)))

(defun week+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns ti) (* *week-ns* delta))))

(defhelp week+
    (use "(week+ dateli n) => dateli")
  (info "Adds #n weeks to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (sec+ minute+ hour+ day+ month+ year+ now)))

(defun month+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns (list
					      (list (1st (1st ti))
						    (+ (2nd (1st ti) 1) delta)
						    (3rd (1st ti) 1))
					      (list (1st (2nd ti nil) 12)
						    (2nd (2nd ti nil) 0)
						    (3rd (2nd ti nil) 0)
						    (4th (2nd ti nil) 0)))))))

(defhelp month+
    (use "(month+ dateli n) => dateli")
  (info "Adds #n months to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (sec+ minute+ hour+ day+ week+ year+ now)))

(defun year+ (ti delta)
  (epoch-ns->datelist (+ (datelist->epoch-ns (list
					      (list (+ (1st (1st ti)) delta)
						    (2nd (1st ti) 1)
						    (3rd (1st ti) 1))
					      (list (1st (2nd ti nil) 12)
						    (2nd (2nd ti nil) 0)
						    (3rd (2nd ti nil) 0)
						    (4th (2nd ti nil) 0)))))))

(defhelp year+
    (use "(month+ dateli n) => dateli")
  (info "Adds #n years to the given date #dateli in datelist format and returns the new datelist.")
  (type proc)
  (arity 2)
  (see (sec+ minute+ hour+ day+ week+ month+ now)))

(defun add1 (n)
  (+ n 1))

(defhelp add1
    (use "(add1 n) => num")
  (info "Add 1 to number #n.")
  (type proc)
  (arity 1)
  (see (sub1 + -)))

(defun sub1 (n)
  (- n 1))

(defhelp sub1
    (use "(sub1 n) => num")
  (info "Subtract 1 from #n.")
  (type proc)
  (arity 1)
  (see (add1 + -)))

(defun lcons (datum li)
  (nreverse (cons datum (nreverse li))))

(defhelp lcons
    (use "(lcons datum li) => list")
  (info "Insert #datum at the end of the list #li. There may be a more efficient implementation of this in the future. Or, maybe not. Who knows?")
  (type proc)
  (arity 2)
  (see (cons list append nreverse)))

(defun build-list (n proc)
  (_build-list n proc nil))

(defun _build-list (n proc acc)
  (cond
    ((= n 0) acc)
    (t (_build-list (sub1 n) proc (cons (proc n) acc)))))

(defhelp build-list
    (use "(build-list n proc) => list")
  (info "Build a list with #n elements by applying #proc to the counter #n each time.")
  (type proc)
  (arity 2)
  (see (list list? map foreach)))

(defun get-partitions (x n)
  (lambda (&rest idx)
    (cond
      ((null? idx) (count-partitions x n))
      (t (if (or (< (1st idx -1) 0) (>= (1st idx -1) (count-partitions x n)))
	     nil
	     (nth-partition x n (1st idx 0)))))))

(defhelp get-partitions
    (use "(get-partitions x n) => proc/1*")
  (info "Return an iterator procedure that returns lists of the form (start-offset end-offset bytes) with 0-index offsets for a given index #k, or nil if there is no corresponding part, such that the sizes of the partitions returned in #bytes summed up are #x and and each partition is #n or lower in size. The last partition will be the smallest partition with a #bytes value smaller than #n if #x is not dividable without rest by #n. If no argument is provided for the returned iterator, then it returns the number of partitions.")
  (type proc)
  (arity 2)
  (see (nth-partition count-partitions get-file-partitions iterate)))

(defun count-partitions (x n)
  (let ((m (div x n)))
    (if (> (% x n) 0)
	(add1 m)
	m)))

(defhelp count-partitions
    (use "(count-partitions m k) => int")
  (info "Return the number of partitions for divding #m items into parts of size #k or less, where the size of the last partition may be less than #k but the remaining ones have size #k.")
  (type proc)
  (arity 2)
  (see (nth-partition get-partitions)))

(defun nth-partition (m k idx)
  (if (= idx (div m k))
      (list (* idx k) (sub1 m) (% m k))
      (list (* k idx) (+ (* k idx) (sub1 k)) k)))

(defhelp nth-partition
    (use "(nth-partition m k idx) => li")
  (info "Return a list of the form (start-offset end-offset bytes) for the partition with index #idx of #m into parts of size #k. The index #idx as well as the start- and end-offsets are 0-based.")
  (type proc)
  (arity 3)
  (see (count-partitions get-partitions)))

(defun iterate (it proc)
  (dotimes (i (it))
    (proc (it i))))

(defhelp iterate
    (use "(iterate it proc)")
  (info "Apply #proc to each argument returned by iterator #it in sequence, similar to the way foreach works. An iterator is a procedure that takes one integer as argument or no argument at all. If no argument is provided, the iterator returns the number of iterations. If an integer is provided, the iterator returns a non-nil value for the given index.")
  (type proc)
  (arity 2)
  (see (foreach get-partitions)))

(defun pathli-absolute? (p)
  (if (null? p)
      nil
      (equal? "/" (car p))))

(defhelp pathli-absolute?
    (use "(pathli-absolute? path) => bool")
  (info "Return true if #path is a path list for an absolute path. These start with a \"/\" as first component.")
  (type proc)
  (arity 1)
  (see (parse-path path-absolute?)))

(defun path-absolute? (fi)
  (pathli-absolute? (cadr (parse-path fi))))

(defhelp path-absolute?
    (use "(path-absolute? fi) => bool")
  (info "Return true if the path component of file or directory #fi is absolute. Special access restrictions apply to absolute paths.")
  (type proc)
  (arity 1)
  (see (pathli-absolute? parse-path)))

(defun interpreter-path (ip)
  (cadr (compose-path nil (_interpreter-path ip))))

(defun _compose-path (proc li acc)
  (cond
    ((null? li) acc)
    (t (_compose-path proc (cdr li) (proc acc (car li))))))

(defun compose-path (fspec &rest pspec)
  (let ((selector
	 (cond
	   ((null? pspec) 'user)
	   ((pathli-absolute? (car pspec)) 'absolute)
	   ((and (list? (car pspec)) (sym? (caar pspec)))
	    (caar pspec))
	   (t 'user))))
    (list
     selector
     (str+
      (cond
	((null? pspec) '(user ""))
	((pathli-absolute? (car pspec))
	 (str+ "/"
	       (_compose-path (lambda (s x) (str+ s x "/"))
			      (cdar pspec) "")))
	((and (list? (car pspec)) (sym? (caar pspec)))
	 (_compose-path (lambda (s x) (str+ s x "/"))
			(cdar pspec) ""))
	(t (_compose-path (lambda (s x) (str+ s x "/"))
			  (car pspec) "")))
      (cond
	((null? fspec) "")
	((null? (cdr fspec)) (car fspec))
	(t (str+ (car fspec) "." (cadr fspec))))))))

(defhelp compose-path
    (use "(compose-path [fspec] [pspec]) => str")
  (info "Compose a filepath of the form '(selector string-path) out of the file list #fspec and the path list #pspec. At least one of [fspec] and [pspec] must be specified.")
  (type proc)
  (arity -2)
  (see (parse-path path-absolute? pathli-absolute?)))

(defun del-pathsep (p)
  (cond
    ((equal? p "") "")
    ((equal? (last p) 47) (slice p 0 (sub1 (len p))))
    (t p)))

(defhelp del-pathsep
    (use "(del-pathsep p) => str")
  (info "Remove the trailing path separator from the string path #p if there is one, and return the shortened path. If the path is empty or it doesn't and in a path separator, #p is returned.")
  (type proc)
  (arity 1)
  (see (dexists?)))

;; check whether a sys key is present
(defun sys-key? (k)
  (let ((s (gensym)))
    (if (equal? (sys k s) s) nil t)))

(defhelp sys-key?
    (use "(sys-key? key) => bool")
  (info "Return true if the given sys key #key exists, nil otherwise.")
  (type proc)
  (arity 1)
  (see (sys setsys)))

;; center a string
(defun strcenter (s n)
  (let ((x (truncate (/ (- n (len s)) 2))))
    (str+ (strbuild " " x) s (strbuild " " x))))

(defhelp strcenter
    (use "(strcenter s n) => str")
  (info "Center string #s by wrapping space characters around it, such that the total length the result string is #n.")
  (type proc)
  (arity 2)
  (see (strleft strright strlimit)))

;; align a string left
(defun strleft (s n)
  (str+ s (strbuild " " (- n (len s)))))

(defhelp strleft (s n)
  (use "(strleft s n) => str")
  (info "Align string #s left by adding space characters to the right of it, such that the total length the result string is #n.")
  (type proc)
  (arity 2)
  (see (strcenter strright strlimit)))

;; align a string right
(defun strright (s n)
  (str+ (strbuild " " (- n (len s))) s))

(defhelp strright (s n)
  (use "(strright s n) => str")
  (info "Align string #s right by adding space characters in front of it, such that the total length the result string is #n.")
  (type proc)
  (arity 2)
  (see (strcenter strleft strlimit)))

;; return a string of n spaces
(defun spaces (n)
  (strbuild " " n))

(defhelp spaces
    (use "(spaces n) => str")
  (info "Create a string consisting of #n spaces.")
  (type proc)
  (arity 1)
  (see (strbuild strleft strright)))

;; limit the length of a string to n or less
(defun strlimit (s n)
  (if (> (len s) n)
      (slice s 0 n)
      s))

(defhelp strlimit
    (use "(strlimit s n) => str")
  (info "Return a string based on #s cropped to a maximal length of #n (or less if #s is shorter).")
  (type proc)
  (arity 2)
  (see (strcenter strleft strright)))

;; return the formated date string in ISO format
(defun datestr (d)
  (cond
    ((null? (cdr d)) (fmt "%v-%v-%v" (caar d) (cadar d) (caddr (car d))))
    (t (fmt "%v-%v-%v %v:%02v" (caar d) (cadar d) (caddr (car d))
	    (caadr d) (cadr (cadr d))))))

(defhelp datestr
    (use "(datestr datelist) => str")
  (info "Return datelist, as it is e.g. returned by (now), as a string in format YYYY-MM-DD HH:mm.")
  (type proc)
  (arity 1)
  (see (now datestr* datestr->datelist)))

(defun datestr* (d)
  (cond
    ((null? (cdr d)) (fmt "%v-%v-%v" (caar d) (cadar d) (caddr (car d))))
    (t (fmt "%v-%v-%v %v:%02v:%02v.%v" (caar d) (cadar d) (caddr (car d))
	    (1st (2nd d) 0) (2nd (2nd d) 0) (3rd (2nd d) 0)  (4th (2nd d) 0)))))

(defhelp datestr*
    (use "(datestr* datelist) => str")
  (info "Return the datelist, as it is e.g. returned by (now), as a string in format YYYY-MM-DD HH:mm:ss.nanoseconds.")
  (type proc)
  (arity 1)
  (see (now datestr datestr->datelist)))

(defun datestr->datelist (s)
  (letrec ((parts (strsplit s " "))
	   (dstr (array-ref parts 0))
	   (tmstr (if (> (len parts) 1)(array-ref parts 1) "12:00:00.000000"))
	   (dd (strsplit dstr "-"))
	   (tt (strsplit tmstr ":"))
	   (nn (strsplit (if (> (len tt) 2) (array-ref tt 2) "0.0") "."))
	   (year (str->expr (array-ref dd 0)))
	   (month (str->expr (array-ref dd 1)))
	   (day (str->expr (array-ref dd 2)))
	   (hour (str->expr (array-ref tt 0)))
	   (minute (str->expr (array-ref tt 1)))
	   (sec (str->expr (array-ref nn 0)))
	   (ns (str->expr (if (> (len nn) 1) (array-ref nn 1) "0"))))
    (list (list year month day (day-of-week year month day)(week-of-date year month day))
	  (list hour minute sec ns (date->epoch-ns year month day hour minute sec ns)))))

(defhelp datestr->datelist
    (use "(datestr->datelist s) => li")
  (info "Convert a date string in the format of datestr and datestr* into a date list as it is e.g. returned by (now).")
  (type proc)
  (arity 1)
  (see (datestr* datestr now)))

(defun nl ()
  (out "\n"))

(defhelp nl
    (use "(nl)")
  (info "Display a newline, advancing the cursor to the next line.")
  (type proc)
  (arity 0)
  (see (out outy output-at)))

(defun abs (x)
  (if (< x 0)
      (* x -1)
      x))

(defhelp abs
    (use "(abs x) => num")
  (info "Returns the absolute value of number #x.")
  (type proc)
  (arity 1)
  (see (* - + /)))

;; STYLED TEXT

;; a bunch of colors (web color names)
(setq *colors*
      (dict
       '(pink (255 192 203)
	 light-pink (255 182 193)
	 hot-pink (255 105 180)
	 deep-pink (255  20 147)
	 pale-violet-red (219 112 147)
	 medium-violet-red (199  21 133)
	 light-salmon (255 160 122)
	 salmon (250 128 114)
	 dark-salmon (233 150 122)
	 light-coral (240 128 128)
	 indian-red (205  92  92)
	 crimson (220  20  60)
	 firebrick (178  34  34)
	 dark-red (139   0   0)
	 red (255   0   0)
	 orange-red (255  69   0)
	 tomato (255  99  71)
	 coral (255 127  80)
	 dark-orange (255 140   0)
	 orange (255 165   0)
	 yellow (255 255   0)
	 light-yellow (255 255 224)
	 lemon-chiffon (255 250 205)
	 light-goldenrod-yellow (250 250 210)
	 papaya-whip (255 239 213)
	 moccasin (255 228 181)
	 peach-puff (255 218 185)
	 pale-goldenrod (238 232 170)
	 khaki (240 230 140)
	 dark-khaki (189 183 107)
	 gold (255 215   0)
	 cornsilk (255 248 220)
	 blanched-almond (255 235 205)
	 bisque (255 228 196)
	 navajo-white (255 222 173)
	 wheat (245 222 179)
	 burlywood (222 184 135)
	 tan (210 180 140)
	 rosy-wood (188 143 143)
	 sandy-brown (244 164  96)
	 goldenrod (218 165  32)
	 dark-goldenrod (184 134  11)
	 peru (205 133  63)
	 chocolate (210 105  30)
	 saddle-brown (139  69  19)
	 sienna (160  82  45)
	 brown (165  42  42)
	 maroon (128   0   0)
	 dark-olive-green (85 107  47)
	 olive (128 128   0)
	 olive-drab (107 142  35)
	 yellow-green (154 205  50)
	 lime-green (50 205  50)
	 lime ( 0 255   0)
	 lawn-green (124 252   0)
	 chartreuse (127 255   0)
	 green-yellow (173 255  47)
	 spring-green ( 0 255 127)
	 medium-spring-green ( 0 250 154)
	 light-green (144 238 144)
	 pale-green (152 251 152)
	 dark-sea-green (143 188 143)
	 medium-aquamarine (102 205 170)
	 medium-sea-green ( 60 179 113)
	 sea-green ( 46 139  87)
	 forrest-green ( 34 139  34)
	 green ( 0 128   0)
	 dark-green (  0 100   0)
	 aqua (0 255 255)
	 cyan ( 0 255 255)
	 light-cyan (224 255 255)
	 pale-turquoise (175 238 238)
	 aquamarine (127 255 212)
	 turquoise (64 224 208)
	 medium-turquoise (72 209 204)
	 dark-turquoise (0 206 209)
	 light-sea-green (32 178 170)
	 cadet-blue (95 158 160)
	 dark-cyan (0 139 139)
	 teal (0 128 128)
	 light-steel-blue (176 196 222)
	 powder-blue (176 224 230)
	 light-blue (173 216 230)
	 sky-blue (135 206 235)
	 light-sky-blue (135 206 250)
	 deep-sky-blue (0 191 255)
	 dodger-blue (30 144 255)
	 cornflower-blue (100 149 237)
	 steel-blue (70 130 180)
	 royal-blue (65 105 225)
	 blue (0   0 255)
	 medium-blue (0   0 205)
	 dark-blue (0   0 139)
	 navy (0   0 128)
	 midnight-blue (25  25 112)
	 lavender (230 230 250)
	 thistle (216 191 216)
	 plum (221 160 221)
	 violet (238 130 238)
	 orchid (218 112 214)
	 fuchsia (255   0 255)
	 magenta (255   0 255)
	 medium-orchid (186  85 211)
	 medium-purple (147 112 219)
	 blue-violet (138  43 226)
	 dark-violet (148   0 211)
	 dark-orchid (153  50 204)
	 dark-magenta (139   0 139)
	 purple (128   0 128)
	 indigo (75   0 130)
	 dark-slate-blue (72  61 139)
	 slate-blue (106  90 205)
	 medium-slate-blue (123 104 238)
	 white (255 255 255)
	 snow (255 250 250)
	 honeydew (240 255 240)
	 mint-cream (245 255 250)
	 azure (240 255 255)
	 alice-blue (240 248 255)
	 ghost-white (248 248 255)
	 white-smoke (245 245 245)
	 seashell (255 245 238)
	 beige (245 245 220)
	 old-lace (253 245 230)
	 floral-white (255 250 240)
	 ivory (255 255 240)
	 antique-white (250 235 215)
	 linen (250 240 230)
	 lavender-blush (255 240 245)
	 misty-rose (255 228 225)
	 gainsboro (220 220 220)
	 light-gray (211 211 211)
	 silver (192 192 192)
	 dark-gray (169 169 169)
	 gray (128 128 128)
	 dim-gray (105 105 105)
	 light-slate-gray (119 136 153)
	 slate-gray (112 128 144)
	 dark-slate-gray (47  79  79)
	 black (0   0   0)
	 ;; proprietary
	 z3s5-orange (200 100 0 180)
	 z3s5-blue (0 0 60 255)
	 z3s5-help-text text
	 z3s5-help-back back
	 z3s5-error-text (120 0 0 255)
	 z3s5-error-back back
	 z3s5-warn-text (160 94 35 255)
	 z3s5-warn-back back
	 z3s5-help-entry-text (255 255 255 255)
	 z3s5-help-entry-arg (0 0 120 255)
	 z3s5-help-entry-back (0 80 0 255)
	 z3s5-help-warn-text (120 0 0 255)
	 z3s5-help-warn-back back
	 z3s5-log-warn-text (160 94 35 255)
	 z3s5-log-warn-back back
	 z3s5-log-error-text (120 0 0 255)
	 z3s5-log-error-back back
	 z3s5-sysmsg-text (255 255 255 180)
	 z3s5-sysmsg-back back
	 z3-title-fg-0 (255 255 255 255)
	 z3-title-fg-1 (0 0 0 255)
	 z3-title-fg-2 (0 0 0 255)
	 z3-title-fg-3 (0 0 0 255)
	 z3-title-fg-4 (0 0 0 255)
	 z3-title-fg-5 (0 0 0 255)
	 z3-title-fg-6 (0 0 0 255)
	 z3-title-bg-0 (0 0 0 255)
	 z3-title-bg-1 back
	 z3-title-bg-2 back
	 z3-title-bg-3 back
	 z3-title-bg-4 back
	 z3-title-bg-5 back
	 z3-title-bg-6 back
	 z3-text-fg (220 220 220 255)
	 z3-text-bg back
	 z3-link-fg (0 0 120 255)
	 z3-link-bg back
	 z3-linknum-fg (100 100 250 255)
	 z3-linknum-bg (0 0 120 255)
	 )))

(defhelp *colors*
    (use "*colors*")
  (info "A global dict that maps default color names to color lists (r g b), (r g b a) or selectors for (color selector). This can be used with procedure the-color to translate symbolic names to colors.")
  (type dict)
  (arity 0)
  (see (the-color)))

;; get a color by specification, which is either a name symbol, a list of (r g b) or a list of (r g b a)
;; returns the full color (r g b a)
(defun the-color (spec)
  (cond
    ((sym? spec)
     (let ((c (get *colors* spec nil)))
       (if c
	   (cond
	     ((sym? c) (color c))
	     ((> (len c) 3) c)
	     (t (nreverse (cons 255 (nreverse c)))))
	   (error "color not in *colors*: %v" spec))))
    ((list? spec)
     (cond
       ((< (len spec) 3) (error "expected a list of r g b values, given %v" spec))
       ((= (len spec) 3) (nreverse (cons 255 (nreverse spec))))
       ((> (len spec) 3) spec)
       (t (error "malformed color list: %v" spec))))
    (t (error "malformed color specification: %v" spec))))

(defhelp the-color
    (use "(the-color colors-spec) => (r g b a)")
  (info "Return the color list (r g b a) based on a color specification, which may be a color list (r g b), a color selector for (color selector) or a color name such as 'dark-blue.")
  (type proc)
  (arity 1)
  (see (*colors* color set-color outy)))

(defun the-color-names ()
  (dict->keys *colors*))

(defhelp the-color-names
    (use "(the-color-names) => li")
  (info "Return the list of color names in *colors*.")
  (type proc)
  (arity 0)
  (see (*colors* the-color)))

;; print styled text helpers
(defun _outy1 (te)
  (when (car te)
    (case (car te)
      ((fg)
       (let ((fg (color 'text)))
	 (try ((set-color 'text fg))
	      (set-color 'text (the-color (cadr te)))
	      (outy1 (caddr te)))))
      ((bg)
       (let ((bg (color 'back)))
	 (try ((set-color 'back bg))
	      (set-color 'back (the-color (cadr te)))
	      (outy1 (caddr te)))))
      ((text)
       (dolist (unstyled (cdr te) (void)) (out unstyled)))
      (t (error "unknown styled text tag: %v" (car te))))
    (void)))

(defun outy1 (te)
  (if (list? te)
      (_outy1 te)
      (out te)))

;; styled output: (fg color ...) foreground, (bg color ...) background, (text "string" ...) text
(defun outy (li)
  (dolist (styled li (void))
    (outy1 styled)))

(defhelp outy
    (use "(outy spec)")
  (info "Output styled text specified in #spec. A specification is a list of lists starting with 'fg for foreground, 'bg for background, or 'text for unstyled text. If the list starts with 'fg or 'bg then the next element must be a color suitable for (the-color spec). Following may be a string to print or another color specification. If a list starts with 'text then one or more strings may follow.")
  (type proc)
  (arity 1)
  (see (*colors* the-color set-color color gfx.color output-at out)))

(defun synout (arg)
  (enq
   (lambda ()
     (out arg)
     (when (permission? 'interactive (read-eval-reply))))))

(defhelp synout
    (use "(synout arg)")
  (info "Like out, but enforcing a new input line afterwards. This needs to be used when outputing concurrently in a future or task.")
  (type proc)
  (arity 1)
  (see (out outy synouty))
  (warn "Concurrent display output can lead to unexpected visual results and ought to be avoided."))

(defun synouty (li)
  (enq
   (lambda ()
     (outy li)
     (when (permission? 'interactive) (read-eval-reply)))))

(defhelp synouty
    (use "(synouty li)")
  (info "Like outy, but enforcing a new input line afterwards. This needs to be used when outputing concurrently in a future or task.")
  (type proc)
  (arity 1)
  (see (synout out outy))
  (warn "Concurrent display output can lead to unexpected visual results and ought to be avoided."))

(defun sysmsg (msg)
  (cond
    ((member (sys 'editmode) '(page console))
     (enq (lambda ()
	    (synouty
	     (list
	      (list 'fg (the-color 'z3s5-sysmsg-text)
		    (list 'bg (the-color 'z3s5-sysmsg-back)
			  (fmt "%v\n" msg))))))))
    (t (log msg))))

(defhelp sysmsg
    (use "(sysmsg msg)")
  (info "Asynchronously display a system message string #msg if in console or page mode, otherwise the message is logged.")
  (type proc)
  (arity 1)
  (see (sysmsg* synout synouty out outy)))

(defun sysmsg* (msg)
  (cond
    ((member (sys 'editmode) '(page console))
     (synouty
      (list
       (list 'fg (the-color 'z3s5-sysmsg-text)
	     (list 'bg (the-color 'z3s5-sysmsg-back)
		   (fmt "%v\n" msg))))))
    (t (log msg))))

(defhelp sysmsg*
    (use "(sysmsg* msg)")
  (info "Display a system message string #msg if in console or page mode, otherwise the message is logged.")
  (type proc)
  (arity 1)
  (see (sysmsg synout synouty out outy)))

;; sort a list of symbols lexicographically
(defun sort-symbols (li)
  (sort li
	(lambda (x y)
	  (strless (sym->str x) (sym->str y)))))

(defhelp sort-symbols
    (use "(sort-symbols li) => list")
  (info "Sort the list of symbols #li alphabetically.")
  (arity 1)
  (see (out dp du dump)))

(defun dump (&rest arg)
  (let ((all? (2nd arg nil))
	(li (if (list? arg)
		(let ((start (1st arg nil)))
		  (cond
		    ((sym? start)
		     (letrec ((sym0 (sym->str start))
			      (n (len sym0)))
		       (sort-symbols
			(filter
			 (dump-bindings)
			 (lambda (sym)
			   (cond
			     ((> n (len (sym->str sym))) nil)
			     (t (equal? (slice (sym->str sym) 0 n)
					sym0))))))))
		    (t (sort-symbols (dump-bindings)))))
		(t (sort-symbols (dump-bindings))))))
    (if all?
	li
	(filter li (lambda (x) (< (instr (sym->str x) "_") 0))))))

(defhelp dump
    (use "(dump [sym] [all?]) => li")
  (info "Return a list of symbols starting with the characters of #sym or starting with any characters if #sym is omitted, sorted alphabetically. When #all? is true, then all symbols are listed, otherwise only symbols that do not contain \"_\" are listed. By convention, the underscore is used for auxiliary functions.")
  (type proc)
  (arity -1)
  (see (dump-bindings save-zimage load-zimage)))

(defun find-missing-help-entries ()
  (filter (dump) (lambda (sym) (get *help* sym nil))))

(defun find-unneeded-help-entries ()
  (let ((d (dump))
	(r nil))
    (dict-foreach *help* (lambda (k v) (unless (memq k d) (setq r (cons k r)))))
    r))
    
(defun protect-toplevel-symbols ()
  (apply protect (filter (dump-bindings)
			 (lambda (sym)
			   (and (not (protected? sym))
				(not (has-key? *mutable-toplevel-symbols* sym)))))))

(defhelp protect-toplevel-symbols
    (use "(protect-toplevel-symbols)")
  (info "Protect all toplevel symbols that are not yet protected and aren't in the *mutable-toplevel-symbols* dict.")
  (type proc)
  (arity 0)
  (see (protected? protect unprotect declare-unprotected when-permission? dict-protect dict-protected? dict-unprotect)))

;; PREAMBLE END


