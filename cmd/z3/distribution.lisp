;;; DISTRIBUTION
;;;
;;; Automated routines for distribution generation. This file assumes the original directory structure.
;;; See section ACTION below. Caution: This file runs code and modifies files.

;;; Reference Manual
;;; Functions for automated reference manual creation.

(defun help->manual (outfile)
  (out "1. Compiling Reference Manual: ")
  (let ((manual (str+ (_manual-preamble)
		      (fmt "# By Topics\n\n%v\n\n# Complete Reference\n\n%v"
			   (_help->manual-by-topics)
			   (_help->manual-total)))))
    (out "\nDone!\n")
    (out "2. Writing file...")
    (let ((out (open outfile '(write create truncate sync))))
      (write-string out manual)
      (close out))
    (void (out "Done.\n"))))

(defun _manual-preamble ()
  (fmt (str+ "---\n"
	     "title: Z3S5 Lisp Reference Manual\n"
	     "author: by Erich H. Rast, Ph.D., and all other Help system contributors\n"
	     "date: %v\n"
	     "header-includes: |\n"
	     "    \\lstset{%% for listings\n"
	     "        basicstyle=\\footnotesize\\ttfamily,\n"
	     "        breaklines=true,\n"
	     "    }\n"
	     "    \\usepackage{xcolor}\n"
	     "---\n"
	     "\n"
	     "# Introduction\n"
	     "\n"
	     "This is the reference manual for the programming language Z3S5 Lisp, version %v. This manual has been automatically generated from the entries of the online help system. The reference manual is divided into two large sections. Section *By Topics* lists functions and symbols organized by topics. Within each topic, entries are sorted alphabetically. Section *Complete Reference* lists all functions and symbols alphabetically."
	     "\n"
	     "Please consult the *User Manual* and the *Readme* document for more general information about Z3S5 Lisp, an introduction to its use, and how to embedd it into Go programs.\n"
	     "\n"
	     "Incorrect documentation strings are bugs. Please report bugs using the corresponding [Github issue tracker nfor Z3S5 Lisp](https://github.com/rasteric/z3s5-lisp/issues) and make sure to be as precise as possible. Unneeded or missing documentation entries are misfeatures and may also be reported.\n"
	     "\n")  (datestr (now)) (1st (sys 'version) "<unknown>")))

(defun _help->manual-by-topics ()
  (let ((topics (help-topics))
	(s ""))
    (foreach topics
	     (lambda (topic)
	       (setq s (str+ s (_help->topic topic) "\n\n"))))
    s))

(defun _help->topic (topic)
  (let ((symbols (sort-symbols (help-about topic 'first)))
	(info (help-topic-info topic)))
    (if symbols
	(let ((s (fmt "## %v\n\n"
		      (1st info
			   (strcase (sym->str topic) 'title)))))
	  (when info
	    (setq s (str+ s (cadr info) "\n\n")))
	  (foreach symbols
		   (lambda (sym)
		     (let ((entry (help->manual-entry sym 3)))
		       (when entry
			 (setq s (str+ s entry "\n\n"))))))
	  s)
	"")))

(defun _help->manual-total ()
  (let ((symbols (dump))
 	(s ""))
    (foreach symbols
 	     (lambda (sym)
	       (let ((entry (help->manual-entry sym 2)))
		 (when entry
 		   (setq s (str+ s entry "\n\n"))))))
    s))

(defun help->manual-entry (key &rest opt)
  (out key)(out " ")
  (let ((entry (get *help* key nil)))
    (if entry
	(_convert-help-entry key (get *help* key) (if opt (car opt) 3))
	nil)))

(defhelp help->manual-entry
    (use "(help->manual-entry key [level]) => str")
  (info "Looks up help for #key and converts it to a manual section as markdown string. If there is no entry for #key, then nil is returned. The optional #level integer indicates the heading nesting.")
  (arity 1)
  (topic (help))
  (see (help)))

(defun _convert-help-entry (key entry level)
  (if (str? (cadr (assoc 'warn entry)))
      (str+ (_convert-help-entry1 key entry level) "\n\n**Warning: " (cadr (assoc 'warn entry)) "**")
      (_convert-help-entry1 key entry level)))

(defun _convert-help-entry1 (key entry level)
  (fmt "%v %v\n\nUsage: `%v`\n\n%v\n\nSee also: `%v`."
       (_convert-heading level)
       (if (_help-is-functional (cadr (assoc 'type entry)))
	   (fmt "`%v` : %v/%v" key (_help-type-to-str (cadr (assoc 'type entry)))
		(_help-arity-to-str (cadr (assoc 'arity entry))))
	   (fmt "%v : %v" key (_help-type-to-str (cadr (assoc 'type entry)))))
	(cadr (assoc 'use entry))
	(_convert-info (cadr (assoc 'info entry)))
	(_help-see-to-str (cadr (assoc 'see entry)))))

(defun _convert-heading (level)
  (cond
    ((= level 1) "#")
    ((= level 2) "##")
    ((= level 3) "###")
    ((= level 4) "####")
    (t "")))

(defun _convert-info (s)
  (let ((li (_out-help-segment s "#" " .,:()[]{}"))
	(result ""))
    (foreach li
	     (lambda (p)
	       (if (car p)
		   (setq result (str+ result "`" (cadr p) "`"))
		   (setq result (str+ result (cadr p))))))
    result))

;;; ACTION
(help->manual "../../docs/reference/reference.md")
(exit)