;;;; These examples illustrate various user interface components. 

(defun demo1 ()
 (gui 
  (letrec ((win (new-window "Demo 1: Hello world"))
           (button (new-button "Hello world!" (lambda () (close-window win)))))
   (set-window-content win button)
   (set-window-on-close-callback 
    win (lambda () (out (fmt "window %v was closed\n" win))))
   (show-window win))))

(defun demo2 ()
 (gui
  (letrec ((win (new-window "Demo 2: Text Entry"))
           (entry (new-entry)))
   (set-window-content win entry)
   (set-window-on-close-callback 
    win (lambda () (out (fmt "window %v was closed\n" win))))
   (set-entry-on-change-callback entry (lambda (s) (out s)(out "\n")))
   (show-window win))))

(defun demo3 ()
 (gui 
  (letrec ((win (new-window "Demo 3: Radio-Group"))
           (select (new-choice 'radio-group '("Option 1" "Option 2" "Option 3" "Option 4" "Option 5")
                    (lambda (s) (out s)(out "\n")))))
   (set-window-content win select)
   (show-window win))))

(defun demo4 ()
 (gui 
  (letrec ((win (new-window "Demo 4: Forms"))
           (form (new-form)))
   (append-form form "Name" (new-entry))
   (append-form form "Address" (new-entry))
   (append-form form "Phone" (new-entry))
   (append-form form "Email" (new-entry))
   (append-form form "More" (new-hyperlink "Click here for more info" "https://z3s5.com"))
   (set-window-content win form)
   (show-window win))))

(defun demo5 ()
 (gui
  (letrec ((win (new-window "Demo 5: Button with Icon"))
           (button (new-button-with-icon "Press me!" 
		    (theme-icon 'view-restore)
		    (lambda () (close-window win)))))
   (set-window-content win button)
   (show-window win))))

(defun demo6 ()
 (gui
  (letrec ((win (new-window "Demo 6: List"))
           (li (new-list (lambda () 200000)
		(lambda () (new-label "template        "))
		(lambda (lbl idx) (set-label-text lbl (fmt "Item %v" idx))))))
   (set-window-content win li)
   (show-window win))))

(defun demo7 ()
 (gui 
  (letrec ((win (new-window "Demo 7: A Text Grid"))
           (tg (new-text-grid "5 REM\n\n10 PRINT \"Hello world!\"\n20 GOTO 10" 'show-line-numbers)))
   (set-window-content win tg)
   (show-window win))))

(defun demo8 ()
 (gui
  (letrec ((win (new-window "Demo 8: Theme Icons"))
	   (names '(account cancel check-button-checked check-button color-achromatic color-chromatic
		    color-palette computer confirm content-add content-clear content-copy content-cut
		    content-paste content-redo content-remove content-undo delete document-create
		    document-print document download error file-application file-audio file-image
		    file-text file-video file folder-new folder-open folder grid help history home
		    info list login logout mail-attachment mail-compose mail-forward mail-reply-all
		    mail-reply mail-send media-fast-forward media-fast-rewind media-music media-pause
		    media-photo media-play media-record media-replay media-skip-next media-skip-previous
		    media-stop media-video menu-expand menu more-horizontal more-vertical move-down
		    move-up navigate-back navigate-next question radio-button-checked radio-button
		    search-replace search settings storage upload view-full-screen view-refresh
		    view-restore visibility-off visibility volume-down volume-mute volume-up warning))
	   (c (apply new-container
		     (cons (new-grid-layout 10)
			   (map names
				(lambda (name)
				  (new-icon (theme-icon name))))))))
    (set-window-content win c)
    (show-window win))))

(defun demo9 ()
 (gui
  (letrec ((win (new-window "Demo 9: Progress Bar"))
	   (form (new-form))
	   (b1 (new-infinite-progress-bar))
	   (b2 (new-progress-bar))
	   (b3 (new-progress-bar)))
   (append-form form "Finfinite" b1)
   (append-form form "Standard" b2)
   (append-form form "Custom" b3)
   (set-window-content win form)
   (show-window win)
   (set-progress-bar b2 'max 100.0)
   (set-progress-bar b3 'max 100.0)
   (set-progress-bar b3 'formatter (lambda (id) (fmt "Value %v" (get-progress-bar-value id))))
   (dotimes (n 101)
    (sleep 50)
    (set-progress-bar b2 'value n)
    (set-progress-bar b3 n)))))

(defun demo10 ()
 (gui
  (letrec ((win (new-window "Demo 10: Slider"))
	   (label (new-label "<no value yet>"))
	   (slider (new-slider 0.0 1000.0 (lambda (x) (set-label-text label (fmt "Value: %v" x)))))
	   (box (new-border label nil nil nil slider)))
   (set-window-content win box)
   (show-window win))))

(defun demo11 ()
 (gui 
  (letrec ((win (new-window "Demo 11: Table"))
           (li (new-table (lambda () '(200 5))
			 (lambda () (new-label "template        "))
			 (lambda (lbl row col) (set-label-text lbl (fmt "Cell %v-%v" row col))))))
    (set-window-content win li)
    (show-window win))))

(defun demo12 ()
 (gui
  (letrec ((win (new-window "Demo 12: Tree"))
	   (tree (new-tree
		  (lambda (id) (case id
				 (("1 Item") '("1.1 Item" "1.2 Item" "1.3 Item"))
				 (("2 Item") '("2.1 Item" "2.2 Item" "2.4 Item" "2.5 Item"))
				 (("3 Item") '("3.1 Item" "3.2 Item"))
				 (("2.2 Item") '("2.2.1 Item" "2.2.2 Item"))
				 (("") '("1 Item" "2 Item" "3 Item"))
				 (t '())))
		  (lambda (id) (case id
				 (("" "1 Item" "2 Item" "3 Item" "2.2 Item") t)
				 (t nil)))
		  (lambda (is-branch?) (new-label "Tree template"))
		  (lambda (id is-branch? obj) (set-label-text obj id)))))
    (set-window-content win tree)
    (show-window win))))

(defun demo13 ()
 (gui
  (letrec ((win (new-window "Demo 13: Drawing"))
	   (rect (new-rectangle (nrgba 255 100 100 255) 400 400 '(50 50) (nrgba 200 50 50 220) 10.0))
	   (circ (new-circle (nrgba 100 255 100 200) '(100 100) '(700 500) (nrgba 50 200 50 255) 4.0))
	   (line (new-line (nrgba 0 0 0 255) '(0 0) '(800 600) (nrgba 100 100 100 100) 4.0))
	   (text (new-text "This is an example." (nrgba 0 0 200 255)))
	   (canvas (new-container-without-layout rect circ line text)))
   (set-text-style text '(bold italic monospace))
   (set-text-size text 32.0)
   (set-text-alignment text 'center)
   (set-window-content win canvas)
   (set-window-size win 800 600)
   (show-window win)
   (move-object text '(500 300))
   (void (future (dotimes (n 2000)
		  (dotimes (x 100)
		   (gui (resize-object circ (+ 300 x) (+ 300 x)))
		   (sleep 20))
		  (dotimes (x 100)
		   (gui (resize-object circ (- 400 x) (- 400 x)))
		   (sleep 18)))))
   (void (future  (dotimes (n 2000)
		   (let ((pos (get-object-position rect)))
		    (gui (move-object rect (list (+ (car pos) (- (rand 0 1 3) 2))
					    (+ (cadr pos) (- (rand 0 1 3) 2)))))
		    (sleep 70)))))
   (void (future (dotimes (n 2000)
		  (let ((pos (get-object-position text)))
		   (gui (move-object text (list (+ (car pos) (- (rand 0 1 3) 2))
					   (+ (cadr pos) (- (rand 0 1 3) 2)))))
		   (sleep 70))))))))

(defun demo14 ()
 (gui
  (letrec ((win (new-window "Demo 14: Raster Image"))
	   (rast (new-raster-with-pixels (lambda (x y w h)
					  (let ((c (rand 0 0 65535)))
					   (list c c c 65535))))))
   (set-window-content win rast)
   (set-window-size win 100 100)
   (show-window win)
   (void (future (letrec ((refresh (lambda () (gui (refresh-object rast)) (refresh))))
		  (refresh)))))))

(defun demo15 ()
 (gui 
  (letrec ((win (new-window "Demo15: Image"))
	   (img (new-image-from-file "sloth.png")))
    (set-window-content win img)
    (set-window-size win 600 400)
    (show-window win))))

(defun demo16 ()
 (gui 
  (letrec ((win (new-window "Demo16: Validation"))
	   (form (new-form))
	   (e1 (new-entry))
	   (e2 (new-entry))
	   (e3 (new-entry))
	   (e4 (new-entry 'multi-line))
	   (button (new-button "Validate All" (lambda ()
						(set-entry-text
						 e4
						 (str+
						  "Field 1: "
						 (validate-object e1)
						 "\nField 2: "
						 (validate-object e2)
						 "\nField 3: "
						(validate-object e3)))))))
    (set-entry-validator e1 (new-validator (lambda (s) (if (not (equal? s "schmoopie"))
							   "This entry only accepts the string \"schmoopie\"."
							   ""))))
    (set-entry-validator e2 (new-time-validator "Mon Jan 2 15:04:05 -0700 MST 2006"))
    (set-entry-validator
     e3
     (new-regexp-validator
      "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
      "This is not a valid email address."))
    (append-form form "Field 1" e1)
    (append-form form "Field 2" e2)
    (append-form form "Field 3" e3)
    (append-form form "Manual Validation" button)
    (append-form form "Validation Result" e4)
    (set-window-content win form)
    (set-window-size win 800 400)
    (show-window win))))

(defun demo17 ()
 (gui 
  (letrec ((win (new-window "Demo 17: Extended Text Grid"))
	   (grid (new-text-grid)))
   (set-window-content win grid)
   (show-window win)
   (void (future (letrec ((doit
			   (lambda ()
			    (let ((bw (rand 0 0 255)))
			     (gui (set-text-grid-style
				   grid (rand 0 0 19) (rand 0 0 99)
				   (list (list 0 0 0 255)
				    (list bw bw bw 255))))
			     (doit)))))
		  (doit)))))))

(defun demo18 ()
 (gui 
  (letrec ((win (new-window "Demo 18: Keyboard"))
	   (lb (new-label "     <type something>     ")))
   (set-window-content win lb)
   (set-canvas-on-typed-key (get-window-canvas win) (lambda (sym scancode)
						     (set-label-text
						      lb
						      (fmt "key: %v code: %v" sym scancode))))
   (show-window win))))

(defun demo19 ()
 (gui 
  (letrec ((win (new-window "Demo 19: Split"))
	   (e1 (new-entry 'multi-line))
	   (e2 (new-entry 'multi-line))
	   (split (new-vsplit e1 e2)))
    (set-entry-place-holder e2 "Enter some Lisp symbol to look up")
    (set-entry-text-wrap e1 'word)
    (set-entry-on-change-callback
     e2 (lambda (s)
	  (let ((sym (str->sym s)))
	    (cond
	      ((_bound? sym)
	       (let ((h (help-entry sym)))
		 (if h
		     (set-entry-text e1
				     (str+
				      (assoc1 'use h)
				      "\n\n"
				      (assoc1 'info h)
				      "\n\n"
				      (fmt "See also: %v" (assoc1 'see h))))
		     (set-entry-text e1
				     (fmt "%v\n\nNo help available, value: %v" sym (eval sym))))))
	      (t (set-entry-text e1 (fmt "Not bound: %v" sym))))))) 
    (set-window-content win split)
    (set-split-offset split 0.8)
    (focus-canvas-object (get-window-canvas win) e2)
    (set-window-size win 800 600)
    (show-window win))))

(defun demo20 ()
 (gui 
  (letrec ((win (new-window "Demo 20: Clumsy Focus Monitor"))
	   (form (new-form))
	   (e1 (new-entry))
	   (e2 (new-entry))
	   (e3 (new-entry))
	   (lb (new-label "<focus unknown>"))
	   (c (get-window-canvas win))
	   (b (new-button "Unfocus" (lambda () (unfocus-canvas-objects c)))))
    (append-form form "Entry 1" e1)
    (append-form form "Entry 2" e2)
    (append-form form "Entry 3" e3)
    (append-form form "Current Focus:" lb)
    (append-form form "" b)
    (set-window-content win form)
    (future (letrec ((doit (lambda ()
			     (gui (set-label-text lb (fmt "%v" (get-focused-canvas-object c))))
			     (sleep 1000)
			     (doit))))
	      (doit)))
    (set-window-size win 400 180)
    (show-window win))))

(defun demo21 ()
 (gui 
  (letrec ((win (new-window "Demo 21: Typewriter 1.0"))
	   (info (new-label "Single line only. Type \ to delete everything!"))
	   (lb (new-label "_"))
	   (border (new-border info nil nil nil lb)))
    (set-window-content win border)
    (set-canvas-on-typed-rune
     (get-window-canvas win)
     (lambda (s)
       (if (equal? s "\\")
	   (set-label-text lb "_")
	   (letrec ((txt (get-label-text lb)))
	     (set-label-text
	      lb
	      (str+ (str-remove-suffix txt "_") s "_"))))))
    (set-window-size win 600 40)
    (show-window win))))

(defun demo22 ()
 (gui 
  (letrec ((win (new-window "Demo 22: Shortcuts"))
	   (info (new-label "Use shortcuts Ctrl-S to Ctrl-F."))
	   (canvas (get-window-canvas win))
	   (b (new-button "Remove Ctrl-F" (lambda () (remove-canvas-shortcut canvas '(ctrl f)))))
	   (form (new-form)))
    (append-form form "Pressed:" info)
    (append-form form "" b)
    (set-window-content win form)
    (add-canvas-shortcut canvas '(ctrl s) (lambda () (set-label-text info "Ctrl-S")))
    (add-canvas-shortcut canvas '(ctrl d) (lambda () (set-label-text info "Ctrl-D")))
    (add-canvas-shortcut canvas '(ctrl f) (lambda () (set-label-text info "Ctrl-F")))
    (show-window win))))

(defun demo23 ()
 (gui 
  (letrec ((win (new-window "Demo 23: Approximate Pi"))
	   (lbl (new-label "This program approximates pi."))
	   (steps (new-entry))
	   (result (new-entry))
	   (button (new-button "Calculate!" (lambda () (void))))
	   (form (new-form))
	   (border (new-border lbl button nil nil form)))
    (set-entry-text steps (fmt "%v" 100000))
    (append-form form "Steps: " steps)
    (append-form form "Result: " result)
    (set-window-content win border)
    (show-window win))))

(defun demo24 ()
 (gui 
  (letrec ((win (new-window "Demo 24: Text-grid"))
	   (s (apply str+ (build-list 1000 (lambda (x) (str+ (create-lorem-ipsum 'sentence 3 20) "\n")))))
           (tg (new-text-grid s 'show-line-numbers)))
    (set-window-content win tg)
    (show-window win))))

(defun demo25 ()
 (gui 
  (letrec ((win (new-window "Demo 25: ZEdit"))
	   (s (apply str+ (build-list 1000 (lambda (x) (str+ (create-lorem-ipsum 'sentence 3 20) "\n")))))
           (ed (new-zedit 80 40 (get-window-canvas win))))
    (set-window-content win ed)
    (set-zedit-config ed 'show-line-numbers? true)
    (focus-canvas-object (get-window-canvas win) ed)
    (set-zedit-text ed s)
    (show-window win))))

(defun demo26 ()
 (gui 
  (letrec ((win (new-window "Demo 26: Color Picker"))
	   (rect (new-rectangle (nrgba 255 100 100 255))))
    (set-window-content win rect)
    (show-window win)
    (set-window-size win 800 600)
    (show-color-picker "Color Choice" "Choose a color:"
		       (lambda (c)
			 (set-window-content win (new-rectangle (apply nrgba c))))
		       win))))

(defun demo27 ()
 (gui 
  (letrec ((win (new-window "Demo 27: Various Small Dialogs"))
	   (form (new-form))
	   (b1 (new-button "Confirm" (lambda ()
				       (show-confirm "Confirm"
						     "Do you really want do this?"
						     (lambda (b) (out b)(nl))
						     win))))
	   (b2 (new-button "Open" (lambda ()
				    (show-file-open (lambda (p err)
						      (out p)(nl)
						      (out err)(nl))
						    win))))
	   (b3 (new-button "Open Folder" (lambda ()
					   (show-folder-open (lambda (uri err)
							       (cond
								 (uri 
								  (out (list-uri uri))
								  (nl)
								  (close uri))
								 (t (out "canceled"))))
							     win))))
	   (b4 (new-button "Save" (lambda ()
				    (show-file-save (lambda (uri err)
						      (cond
							(uri
							 (out uri)(nl)
							 (close uri))
							(t (out "canceled")(nl))))
						    win))))
	   (b5 (new-button "Show Info" (lambda ()
					 (show-information "Some Information"
							   "This is some information dialog to show, as the name implies,\nsome information to the user."
							   win))))
	   (b6 (new-button "Show Form" (lambda ()
					 (show-form "Hire New Employee"
						    "Hire"
						    "Cancel"
						    `(,(new-form-item "Name" (new-entry) "e.g. John Smith") ,(new-form-item "Age" (new-entry) "e.g. 21")
						       ,(new-form-item "Role" (new-entry) "e.g. accountant"))
						    (lambda (confirmed?)
						      (out confirmed?)(nl))
						    win))))
	   (b7 (new-button "Show Custom" (lambda ()
					   (show-custom "Enter Code" "OK" (new-entry) win))))
	   (b8 (new-button "Show Custom Confirm" (lambda ()
						   (show-custom-confirm "Enter Something" "OK" "Cancel" (new-entry) (lambda (ok) (out ok)(nl)) win)))))
    (append-form form "(show-confirm title msg proc win)" b1)
    (append-form form "(show-file-open proc win)" b2)
    (append-form form "(show-folder-open proc win)" b3)
    (append-form form "(show-file-save proc win)" b4)
    (append-form form "(show-information title message win)" b5)
    (append-form form "(show-form title confirm dismiss items proc win)" b6)
    (append-form form "(show-custom title dismiss widget win)" b7)
    (append-form form "(show-custom-confirm title confirm dismiss widget proc win)" b8)
    (set-window-content win form)
    (set-window-size win 800 400)
    (show-window win))))

(out "Use (demo1) ... (demo27) to run GUI demos.\n")

