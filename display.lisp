(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
;(require 'cl-glut-examples)

;(cl-glut-examples:rb-lines)

(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))
  
(defclass lines-window (glut:window) 
  ()
  (:default-initargs 
   :width 400 :height 150 :pos-x 100 :pos-y 100
   :mode '(:single :rgb) :title "chapter.2.5.lisp"))

(defmethod glut:display-window :before ((w lines-window))
  (gl:clear-color #x00ff #x00ff #x00ff 0)
  (gl:shade-model :flat))

;(fmakunbound

(defmethod glut:display ((w lines-window))
  (gl:clear :color-buffer-bit)
  ; white for all lines
  (gl:color 0 0 0)

  ; 1st row, 3 lines, each with a diferent stipple
  (gl:enable :line-stipple)
  (gl:line-stipple 1 #x0101)
  (draw-one-line 50 100 150 100)
  (gl:line-stipple 1 #x00ff)
  (draw-one-line 150 100 250 100)
  (gl:line-stipple 1 #x1c47)
  (draw-one-line 250 100 350 100)
  (gl:disable :line-stipple)

  (gl:flush))

(defmethod glut:reshape ((w lines-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width height 0))

(defmethod glut:keyboard ((w lines-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

(glut:display-window (make-instance 'lines-window))


