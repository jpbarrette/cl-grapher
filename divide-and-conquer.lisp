(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
;(require :cl-graph)

(declaim (optimize (debug 3)))


;(defparameter data '((1 2 3) (2 4 nil) (5 8 9) (6 nil nil) (7 10 11) (8 nil nil) (9 nil nil) (10 nil nil) (11 nil nil)))

(defclass binary-tree-drawer ()
  ((left-contours :accessor left-contours :initarg :left-contours)
   (right-contours :accessor right-contours :initarg :right-contours)
   (data :accessor data :initarg :data)))

(defun make-binary-tree-drawer (data)
  (let ((drawer (make-instance 'binary-tree-drawer
			       :data data 
			       :left-contours (make-hash-table)
			       :right-contours (make-hash-table))))
    (contour drawer (car data))
    drawer))

(defun set-contour (drawer direction vertice-key contour)
    (setf (gethash vertice-key (if (eq direction :left) (left-contours drawer) (right-contours drawer))) contour))

(defun get-contour (drawer direction vertice-key)
    (multiple-value-bind (value has-key) (gethash vertice-key (if (eq direction :left) (left-contours drawer) (right-contours drawer)))
      (if has-key
	  value
	  (progn 
	    (contour drawer (assoc vertice-key data))
	    (gethash vertice-key (if (eq direction :left) (left-contours drawer) (right-contours drawer)))))))

(defun contour (drawer v) 			
  (let ((c-vertice (nth 0 v))
	  (l-vertice (nth 1 v))
	  (r-vertice (nth 2 v)))
      (cond ((and (null l-vertice) (null r-vertice)) 
	     (set-contour drawer :left c-vertice '())
	     (set-contour drawer :right c-vertice ()))
	    ((null l-vertice)
	     (set-contour drawer :left c-vertice (cons -1 (mapcar #'1- (get-contour drawer :left r-vertice))))
	     (set-contour drawer :right c-vertice (cons 1 (mapcar #'1+ (get-contour drawer :right r-vertice)))))
	    ((null r-vertice)
	     (set-contour drawer :left c-vertice (cons 1 (mapcar #'1+ (get-contour drawer :left l-vertice))))
	     (set-contour drawer :right c-vertice (cons -1 (mapcar #'1- (get-contour drawer :right l-vertice)))))
	    (t
	     (let ((l-contour (get-contour drawer :right l-vertice))
		   (r-contour (get-contour drawer :left r-vertice))
		   (distance 2))
	       (mapcar #'(lambda (l r) 
			   (when (> (+ l r 2) distance) 
			     (setf distance (+ l r 2))))
		       l-contour r-contour)
	       (let* ((displacement (/ distance 2))
		      (left-contour (cons displacement (mapcar (lambda (x) (+ displacement x)) (get-contour drawer :left l-vertice))))
		      (right-contour (cons displacement (mapcar (lambda (x) (+ displacement x)) (get-contour drawer :right r-vertice)))))
		 ;; now let add the longer coutour to the smallest one
		 (setf left-contour (append left-contour (mapcar (lambda (x) (- x)) (subseq right-contour (length left-contour)))))
		 (setf right-contour (append right-contour (mapcar (lambda (x) (- x)) (subseq left-contour (length right-contour)))))
		 (set-contour drawer :left c-vertice left-contour)
		 (set-contour drawer :right c-vertice right-contour)))))))
  

(defun draw (drawer)
  (labels ((cp (vertice-key x y)
	     (let* ((vertice (assoc vertice-key (data drawer)))
		    (r-key (nth 2 vertice))
		    (l-key (nth 1 vertice)))
	       (when (not (null l-key))
		 (let ((left-x (- x (car (get-contour drawer :left vertice-key)))))
		   (draw-one-line x y left-x (1+ y))
		   (cp l-key left-x (1+ y))))
	       (when (not (null r-key))
		 (let ((right-x (+ x (car (get-contour drawer :right vertice-key)))))
		   (draw-one-line x y right-x (1+ y))
		   (cp r-key right-x (1+ y)))))))
    (cp (caar (data drawer)) 0 0)))
    

(defclass binary-tree-window (glut:window) 
  ((drawer :accessor drawer :initarg :drawer))
  (:default-initargs 
   :width 400 :height 400 :pos-x 100 :pos-y 100
   :mode '(:single :rgb) :title "chapter.2.5.lisp"))

(defmethod glut:display-window :before ((w binary-tree-window))
  (gl:clear-color #x00ff #x00ff #x00ff 0)
  (gl:shade-model :flat))

(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defmethod glut:display ((w binary-tree-window))
  (gl:clear :color-buffer-bit)
  ; white for all lines
  (gl:color 0 0 0)
  ;; 
  (draw (drawer w))
  (gl:flush))

(defmethod glut:reshape ((w binary-tree-window) width height) ;
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d -10 20 20 -5))

(defmethod glut:keyboard ((w binary-tree-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window)))) ;whi

