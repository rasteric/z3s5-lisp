;;;; z3g tests for the GUI

;;;; BASIC GUI - low level

(testing "gui")

;;; hello world
(expect-ok (setq __a (gui.new-app "com.example")))
(expect-true (gui.app? __a))
(expect-ok (setq __w (gui.new-window __a "Testwindow")))
(expect-true (gui.window? __w))
(expect-ok (setq __label (gui.new-label "Hello world!")))
(expect-ok (gui.window-set-content __w __label))
(expect-ok (gui.window-show __w))
(expect-ok (gui.window-set-master __w))
(expect-ok (gui.window-resize __w (gui.new-size 100 100)))

