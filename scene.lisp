(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)

(load "camera-2")

(declaim (optimize (debug 3)))

(defparameter *debug* nil)

(defun is-debug ()
  *debug*)

(defun toggle-debug ()
  (setf *debug* (not (is-debug))))


(defparameter *points* 
  '((0 0 -10)
    (0 0 10)
    (-10 0 0)
    (10 0 0)
    (0 -10 0)
    (0 10 0)))
  
(defparameter *gen-list* nil)
(defparameter *gen-list-initialized* nil)

(defun draw-vertice (p)
  (format t "vertice ~A~%" p)
  (gl:color 0.0 0.3 0.0)
  (gl:with-pushed-matrix 
    (apply #'gl:translate p)
    (gl:call-list *gen-list*)))

(defun draw-one-line (p1 p2)
  (gl:color 0.2 0 0.1)
  (gl:with-primitives :lines
    (apply #'gl:vertex p1)
    (apply #'gl:vertex p2)))


(defun draw ()
  (unless *gen-list-initialized*
    (setf *gen-list* (gl:gen-lists 1))
    (setf *gen-list-initialized* t))
  (let ((qobj (glu::new-quadric))) 
    (gl:with-new-list (*gen-list* :compile)
      (glu::disk qobj 0 2 32 32))
    (glu::delete-quadric qobj))
  ; draw lines
  (mapl (lambda (points)
	  (let ((p (car points))
		(rest (cdr points)))
	    ;; draw a line betneen
	    (mapc (lambda (remote-p)
		    (draw-one-line p remote-p)) rest)))
	*points*)
  ; draw vertices
  (mapcar (lambda (p)
	    (draw-vertice p))
	  *points*))
  ;(gl:delete-lists *gen-list* 1))

(defclass gl-window (glut:window) 
  ()
  (:default-initargs 
   :width 800 :height 600 :pos-x 100 :pos-y 100 :tick-interval #.(floor (* 1000 (/ 60)))
   :mode '(:double :rgb) :title "chapter.2.5.lisp"))

(defparameter *current-camera* (make-instance 'camera))

(defun init-window ()
  (gl:enable :line-smooth)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:hint :line-smooth-hint :dont-care)
  (look-at *current-camera*))
  
(defmethod glut:display-window :before ((w gl-window))
  (gl:clear-color #x00ff #x00ff #x00ff 0)
  (gl:shade-model :smooth))

(defmethod glut:display ((w gl-window))
  (gl:clear :color-buffer-bit)
  (init-window)
  (draw)
  (glut:swap-buffers))

(defparameter *view-mode* 'ortho)

;(setf *BREAK-ON-SIGNALS* nil)
(defmethod glut:reshape ((w gl-window) width height)
  (gl:viewport 0 0 width height)
  (update-aspect-ratio *current-camera* width height))

(defmethod glut:keyboard ((w gl-window) key x y)
  (declare (ignore x y))
  ;(format t "key: ~A~%" key)
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\q (glut:destroy-current-window))
    (#\z (zoom *current-camera* -5))
    (#\Z (zoom *current-camera* 5))
    (#\d (toggle-debug))))


(defmethod glut:tick ((w gl-window)))




  
  
    




