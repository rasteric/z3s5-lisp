;;;; These examples illustrate various user interface components. 

(defun demo1 ()
  (letrec ((win (new-window "Demo 1: Hello world"))
           (button (new-button "Hello world!" (lambda () (close-window win)))))
    (set-window-content win button)
    (set-window-on-close-callback 
     win (lambda () (out (fmt "window %v was closed\n" win))))
    (show-window win)))

(defun demo2 ()
  (letrec ((win (new-window "Demo 2: Text Entry"))
           (entry (new-entry)))
    (set-window-content win entry)
    (set-window-on-close-callback 
     win (lambda () (out (fmt "window %v was closed\n" win))))
    (set-entry-on-change-callback entry (lambda (s) (out s)(out "\n")))
    (show-window win)))

(defun demo3 ()
  (letrec ((win (new-window "Demo 3: Radio-Group"))
           (select (new-choice 'radio-group '("Option 1" "Option 2" "Option 3" "Option 4" "Option 5") (lambda (s) (out s)(out "\n")))))
    (set-window-content win select)
    (show-window win)))

(defun demo4 ()
  (letrec ((win (new-window "Demo 4: Forms"))
           (form (new-form)))
    (append-form form "Name" (new-entry))
    (append-form form "Address" (new-entry))
    (append-form form "Phone" (new-entry))
    (append-form form "Email" (new-entry))
    (append-form form "More" (new-hyperlink "Click here for more info" "https://z3s5.com"))
    (set-window-content win form)
    (show-window win)))

(defun demo5 ()
  (letrec ((win (new-window "Demo 5: Button with Icon"))
           (button (new-button-with-icon "Press me!" 
					 (theme-icon 'view-restore)
					 (lambda () (close-window win)))))
    (set-window-content win button)
    (show-window win)))

(defun demo6 ()
  (letrec ((win (new-window "Demo 6: List"))
           (li (new-list (lambda () 200000)
			 (lambda () (new-label "template        "))
			 (lambda (lbl idx) (set-label-text lbl (fmt "Item %v" idx))))))
    (set-window-content win li)
    (show-window win)))

(defun demo7 ()
   (letrec ((win (new-window "Demo 7: A Text Grid"))
           (tg (new-text-grid "5 REM\n\n10 PRINT \"Hello world!\"\n20 GOTO 10" 'show-line-numbers)))
    (set-window-content win tg)
    (show-window win)))

(defun demo8 ()
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
    (show-window win)))

(defun demo9 ()
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
      (set-progress-bar b3 n))))

(defun demo10 ()
  (letrec ((win (new-window "Demo 10: Slider"))
	   (label (new-label "<no value yet>"))
	   (slider (new-slider 0.0 1000.0 (lambda (x) (set-label-text label (fmt "Value: %v" x)))))
	   (box (new-border label nil nil nil slider)))
    (set-window-content win box)
    (show-window win)))

(defun demo11 ()
  (letrec ((win (new-window "Demo 11: Table"))
           (li (new-table (lambda () '(200 5))
			 (lambda () (new-label "template        "))
			 (lambda (lbl row col) (set-label-text lbl (fmt "Cell %v-%v" row col))))))
    (set-window-content win li)
    (show-window win)))

(defun demo12 ()
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
    (show-window win)))

(defun demo13 ()
  (letrec ((win (new-window "Demo 13: Drawing"))
	   (rect (new-rectangle (nrgba 255 100 100 255) 200 100 nil (nrgba 200 50 50 220) 10.0))
	   (circ (new-circle (nrgba 100 255 100 200) nil nil (nrgba 50 200 50 255) 4.0))
	   (line (new-line (nrgba 0 0 0 255) nil nil (nrgba 10 10 10 255) 4.0))
	   (text (new-text "This is an example." (nrgba 0 0 200 255)))
	   (canvas (new-container (new-max-layout) rect circ line text)))
    (set-text-style text '(bold italic))
    (set-text-size text 32.0)
    (set-text-alignment text 'center)
    (set-window-content win canvas)
    (set-window-size win 400 200)
    (show-window win)))

(out "Use (demo1) ... (demo13) to run GUI demos.\n")

