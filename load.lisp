(pushnew :darwin-native-glut *features*)

(asdf:operate 'asdf:load-op 'ebd)

(let ((drawer (make-instance 'energy-based-drawer :graph data)))
  (defun main ()
    (glut:display-window (make-instance 'energy-based-window :drawer drawer)))
  (defun reset ()
    (init-drawer drawer)))

;  #+(and sbcl sb-thread)
;  (sb-thread:make-thread (lambda ()
;                           (glut:display-window (make-instance 'energy-based-window :drawer drawer))))
;  #-(and sbcl sb-thread)
;  (glut:display-window (make-instance 'energy-based-window :drawer drawer)))


(progn
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
     (ccl::toplevel))))



#|(ccl:process-run-function
 "OpenGL main thread"
 #'(lambda ()
     (progn
       (ccl::external-call "_CFRunLoopGetCurrent" :address)
       (ccl::external-call
        "__CFRunLoopSetCurrent"
        :address (ccl::external-call "_CFRunLoopGetMain" :address))
       (rlet ((psn #>ProcessSerialNumber))
	     (ccl::external-call "_GetCurrentProcess" :address psn)
	     (with-cstrs ((name "simple OpenGL example"))
	       (ccl::external-call "_CPSSetProcessName" :address psn :address name :void))))
     (main)))|#



#|(defun bound-slot-names (object)
  (let ((class (class-of object)))
    (loop :for slotd in (closer-mop:class-slots class) 
       :when (closer-mop:slot-boundp-using-class class object slotd)
       :collect (closer-mop:slot-definition-name slotd))))|#

