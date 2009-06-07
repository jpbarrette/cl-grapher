(pushnew :darwin-native-glut *features*)

(asdf:operate 'asdf:load-op 'ebd)
#|(require 'closer-mop)|#
#|(sb-ext:gc :full t)|#
(let ((drawer (make-instance 'energy-based-drawer :graph data)))
  (glut:display-window (make-instance 'energy-based-window :drawer drawer)))rr



#|(defun bound-slot-names (object)
  (let ((class (class-of object)))
    (loop :for slotd in (closer-mop:class-slots class) 
       :when (closer-mop:slot-boundp-using-class class object slotd)
       :collect (closer-mop:slot-definition-name slotd))))|#

