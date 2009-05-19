(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
;(require :cl-graph)

(declaim (optimize (debug 3)))



(defclass hv-drawer ()
  ((data :accessor data :initarg :data)
   (coordinates :accessor coordinates :initarg :coordinates))
  (:default-initargs :coordinates (make-hash-table)))

(defmethod initialize-instance :after ((drawer hv-drawer) &rest other-vars)
  (declare (ignore other-vars))
  (build drawer))

(defun set-coordinates (drawer vertice-key offsets)
  (when (and (eq 0 (car offsets)) (eq 0 (cdr offsets)))
    (break))
  (setf (gethash vertice-key (coordinates drawer)) offsets))

(defun get-coordinates (drawer vertice-key)
  (gethash vertice-key (coordinates drawer)))

(defun build (drawer)
  (labels ((cp (vertice-key)
	     ;;; return nb-vertices length
	     (let* ((vertice (assoc vertice-key (data drawer)))
		    (r-key (nth 2 vertice))
		    (l-key (nth 1 vertice)))
	       (cond ((and (null l-key) (null r-key))
		      (values 1 0 0))
		     ((null l-key)
		      (multiple-value-bind (nb-vertices length) (cp r-key)
			(set-coordinates drawer r-key '(1 . 0))
			(values (1+ nb-vertices) (1+ length))))
		     ((null r-key)
		      (multiple-value-bind (nb-vertices length) (cp l-key)
			(set-coordinates drawer l-key '(1 . 0))
			(values (1+ nb-vertices) (1+ length))))
		     (t
		      (multiple-value-bind (l-nb-vertice l-length) (cp l-key)
			(multiple-value-bind (r-nb-vertice r-length) (cp r-key)
			  (if (> r-nb-vertice l-nb-vertice)
			      (progn 
				(set-coordinates drawer r-key (cons (1+ l-length) 0))
				(set-coordinates drawer l-key '(0 . 1))
				(values (+ 1 r-nb-vertice l-nb-vertice) (1+ l-length)))
			      (progn
				(set-coordinates drawer l-key (cons (1+ r-length) 0))
				(set-coordinates drawer r-key '(0 . 1))
				(values (+ 1 r-nb-vertice l-nb-vertice) (1+ r-length)))))))))))
    (cp (caar (data drawer))))
  drawer)

(defun draw (drawer)
  (labels ((d (vertice-key x y)
	     (let* ((vertice (assoc vertice-key (data drawer)))
		    (l-key (nth 1 vertice))
		    (r-key (nth 2 vertice)))
	       (when l-key 
		 (let ((coordinates (get-coordinates drawer l-key)))
		   (draw-one-line x y (+ x (car coordinates)) (+ y (cdr coordinates)))
		   (d l-key (+ x (car coordinates)) (+ y (cdr coordinates)))))
	       (when r-key
		 (let ((coordinates (get-coordinates drawer r-key)))
		   (draw-one-line x y (+ x (car coordinates)) (+ y (cdr coordinates)))
		   (d r-key (+ x (car coordinates)) (+ y (cdr coordinates))))))))

    (d (caar (data drawer)) 0 0)))


(defun draw-vertice ()
  (let ((my-list (gl:gen-lists 1))
	(qobj (glu::new-quadric)))
    (gl:with-new-list (my-list :compile)
      (gl:color 1 0 0)         
      (glu::disk qobj 0 0.2 32 32))
    (gl:call-list my-list)))

(defclass hv-window (glut:window) 
  ((drawer :accessor drawer :initarg :drawer))
  (:default-initargs 
   :width 400 :height 400 :pos-x 100 :pos-y 100
   :mode '(:single :rgb) :title "chapter.2.5.lisp"))

(defmethod glut:display-window :before ((w hv-window))
  (gl:clear-color #x00ff #x00ff #x00ff 0)
  (gl:shade-model :flat))

(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defmethod glut:display ((w hv-window))
  (gl:clear :color-buffer-bit)
  ; white for all lines
  (gl:color 0 0 0)
  ;; 
  (draw (drawer w))
  (gl:flush))

(defmethod glut:reshape ((w hv-window) width height) ;
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d -10 20 20 -5))

(defmethod glut:keyboard ((w hv-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window)))) ;whi


(defparameter data '((1 2 3) (2 4 nil) (3 5 6) (4 nil 7) (5 8 9) (6 nil 12) (7 10 11) (8 nil nil) (9 nil nil) (10 nil nil) (11 nil nil) (12 nil 13) (13 nil nil)))

 (defparameter drawer (make-instance 'hv-drawer :data data))
(glut:display-window (make-instance 'hv-window :drawer drawer))

