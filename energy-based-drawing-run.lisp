(asdf:operate 'asdf:load-op 'ebd)

(let ((drawer (make-instance 'energy-based-drawer :graph data)))
  (glut:display-window (make-instance 'energy-based-window :drawer drawer)))

