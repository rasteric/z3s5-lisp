;;; local init.lisp file
;;; This file is loaded when it is in the same directory as the z3g executable.
;;; Feel free to hack this file as you like!

(init-remember) ;; initializes the remember system, slows down start-up, remove if not needed
(include "test-gui.lisp") ;; include tests for GUI
(include "editor.lisp") ;; include lisp-editor for testing

(defun print-start-banner ()
   (let ((tc (color 'text))
	 (bc (color 'back)))
     (set-color 'text (the-color 'z3s5-sysmsg-text))
     (out "Welcome to ")
     (set-color 'back (the-color 'z3s5-orange))
     (set-color 'text (the-color 'z3s5-blue))
     (out "Z3S5 Lisp")
     (set-color 'back bc)
     (set-color 'text (the-color 'z3s5-sysmsg-text))
     (out " on \"")(out (5th (sys 'version nil) "localhost"))(out "\" (")(out (caddr (sys 'version nil)))(out ")")(out " with ")
     (out (cadr (sys 'version nil)))
     (out " cores!\n")
     (out "The session started on ")(out (datestr (now)))(out " UTC in ")(out (global-startup-time))(out " milliseconds.\n")
     (out "Enter (exit) to close the session. Happy hacking!\n")))

(when *interactive-session* (print-start-banner) (include "demo.lisp"))

;;;  Copyright (c) by Erich Rast
;;;
;;;  The above copyright notice and this permission notice shall be included in
;;;  all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;  DEALINGS IN THE SOFTWARE.
