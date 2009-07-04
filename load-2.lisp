;(pushnew :darwin-native-glut *features*)
(load "scene.lisp")

(defun main ()
  (let ((window (make-instance 'gl-window)))
    #+(and sbcl sb-thread) (sb-thread:make-thread (lambda () (glut:display-window window)))
    #-(and sbcl sb-thread) (glut:display-window window)))

(main)
#|(progn
  (ccl:process-run-function
   "housekeeping"
   #'ccl::housekeeping-loop)
  (ccl:process-interrupt
   ccl::*initial-process*
   (lambda ()
     ;; CCL::%SET-TOPLEVEL is sort of like PROCESS-PRESET for the
     ;; initial process; CCL::TOPLEVEL is sort of like PROCESS-RESET
     ;; for that process.
     (ccl::%set-toplevel
      (lambda ()
       ;;; Set the OSX Window Server's notion of the name of the
       ;;; current process.
       (rlet ((psn #>ProcessSerialNumber))
	 (ccl::external-call "_GetCurrentProcess" :address psn)
         (with-cstrs ((name "simple OpenGL example"))
           (ccl::external-call "_CPSSetProcessName" :address psn :address name :void)))
       (ccl::%set-toplevel nil)
       (main)))
     (ccl::toplevel))))|#

