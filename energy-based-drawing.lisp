(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)

(declaim (optimize (debug 3)))

(defparameter data '((1 2 3) (2 1 3 4) (3 2 1) (4 2 5 6) (5 4 6) (6 4 5)))

(defclass energy-based-drawer () 
  ((data :initarg :data :accessor data)
   (dimension :initarg :dimension :accessor dimension)
   (is-stable :initarg :is-stable :accessor is-stable))
  (:default-initargs :data nil :dimension 2 :is-stable nil))

(defmethod initialize-instance :after ((drawer energy-based-drawer) &rest other)
  (declare (ignore other))
  (let ((data (data drawer)))
    (setf (data drawer) (make-hash-table))
    (mapcar (lambda (vertex)
	      (setf (gethash (car vertex) (data drawer)) (make-instance 'vertex-data :vertex vertex)))
	      ;(format t "initial vertex ~A coordinates: ~A~%" (car vertex) (coordinates (gethash (car vertex) (data drawer)))))
	    data)))


(defclass vertex-data ()
  ((vertex :initarg :vertex :accessor vertex)
   (velocity :initarg :velocity :accessor velocity)
   (coordinates :initarg :coordinates :accessor coordinates))
  (:default-initargs :velocity '(0 0) :coordinates (list (random 5.0) (random 5.0))))

(defun add-force (lhs-force rhs-force)
  (mapcar (lambda (x y) (+ x y)) lhs-force rhs-force))

(defun distance (lhs-points rhs-points)
  "Returns the distance between two points
This is flexible to handle 2 or 3 dimensions  "
  (sqrt (apply '+ (mapcar (lambda (c1 c2)
			    (expt (- c2 c1) 2))
			  lhs-points rhs-points))))

(defparameter *spring-length* 5)
(defparameter *spring-stiffness* 5000)


(defun hooke-attraction-for-component (distance)
  (* (- distance *spring-length*) *spring-stiffness*))


(defun hooke-attraction (lhs-coord rhs-coord)
  (let* ((distance (distance lhs-coord rhs-coord)))
    ;(format t "  hooke-distance ~A~%" distance)
    (mapcar (lambda (c1 c2)
	      (* (- c2 c1) (/ distance) (hooke-attraction-for-component distance)))
	    lhs-coord rhs-coord)))

(hooke-attraction '(0 0) '(0.41931432 0.41931432))

(defparameter *coulomb-constant* 1)
(defun coulomb-repulsion (lhs-coord rhs-coord)
  (let* ((distance (distance lhs-coord rhs-coord))
	 (coulomb-repulsion (/ *coulomb-constant* (expt distance 2))))
    (mapcar (lambda (c1 c2)
	      (* (- c2 c1) (/ distance) coulomb-repulsion))
	    lhs-coord rhs-coord)))

(defparameter *timestep* 0.0001)
(defparameter *damping* 0.2)
(defparameter *mass* 1)

(defun next-step (drawer)
  ;(format t "next-step~%")
  (unless (is-stable drawer)
    (let ((total-kinetic-energy 0)
	  (new-velocities (make-hash-table))
	  (new-coordinates (make-hash-table)))
      ;(format t "process next-step~%")
      (maphash (lambda (key val)
		 (let ((net-force (make-list (dimension drawer) :initial-element 0))
		       (coulomb-repulsion (make-list (dimension drawer) :initial-element 0))
		       (hooke-attraction (make-list (dimension drawer) :initial-element 0)))
		   
		   ;; for each other node.
		   (maphash (lambda (other-key other-val)
			      (unless (eq key other-key)
				(setf coulomb-repulsion (add-force coulomb-repulsion (coulomb-repulsion (coordinates val) (coordinates other-val))))))
			    (data drawer))
		   ;(format t "vertex ~A coulomb-repulsion net-force: ~A~%" key coulomb-repulsion)
		   ;; for each spring connected to this node.
		   (mapcar (lambda (remote-vertex-label) 
			     (let ((h-a (hooke-attraction (coordinates val) (coordinates (gethash remote-vertex-label (data drawer))))))
			       ;(format t " hooke-attraction from ~A to ~A: ~A~%" key remote-vertex-label h-a)
			       (setf hooke-attraction (add-force hooke-attraction h-a)))) 
			   (cdr (vertex val)))
		   ;(format t "vertex ~A hooke-attraction net-force: ~A~%" key hooke-attraction)
		   (setf net-force (add-force coulomb-repulsion hooke-attraction))
		   
		   (setf (gethash key new-velocities) (mapcar (lambda (v n-f)
								(* (+ v (* *timestep* n-f)) *damping*))
							      (velocity val) net-force))
		   (setf (gethash key new-coordinates) (mapcar (lambda (c v)
								 (+ c (* *timestep* v)))
							       (coordinates val) (velocity val)))
		   (setf total-kinetic-energy (+ total-kinetic-energy (* *mass* (sqrt (apply '+ (mapcar (lambda (c) 
													  (expt c 2))
													(gethash key new-velocities)))))))))
	       (data drawer))
      (maphash (lambda (k v)
		 (setf (velocity (gethash k (data drawer))) v))
	       new-velocities)
      (maphash (lambda (k v)
		 ;(format t "vertex ~A new-coordinates: ~A~%" k v)
		 (setf (coordinates (gethash k (data drawer))) v))
	       new-coordinates))))

;;       (when (< total-kinetic-energy 1)
;; 	(setf (is-stable drawer) t)))))

(defun draw (drawer)
  (next-step drawer)
  (maphash (lambda (k v)
	     ;(format t "vertext ~A coordinates: ~A~%" k (coordinates v))
	     (let ((x (car (coordinates v)))
		   (y (cadr (coordinates v))))
	       (mapcar (lambda (o-k)
			 (let* ((other-vertex (gethash o-k (data drawer)))
				(other-x (car (coordinates other-vertex)))
				(other-y (cadr (coordinates other-vertex))))
			   (draw-one-line x y other-x other-y)))
		       (cdr (vertex v)))))
	   (data drawer)))

(defclass energy-based-window (glut:window) 
  ((drawer :accessor drawer :initarg :drawer))
  (:default-initargs 
   :width 800 :height 800 :pos-x 100 :pos-y 100
   :mode '(:single :rgb) :title "chapter.2.5.lisp"))

(defmethod glut:display-window :before ((w energy-based-window))
  (gl:clear-color #x00ff #x00ff #x00ff 0)
  (gl:shade-model :flat))

(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defmethod glut:display ((w energy-based-window))
  (gl:clear :color-buffer-bit)
  ; white for all lines
  (gl:color 0 0 0)
  ;; 
  (draw (drawer w))
  (gl:flush))

(defmethod glut:idle ((w energy-based-window))
  (next-step (drawer w))
  (glut:post-redisplay))

(defmethod glut-reshape ((w energy-based-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d -10 20 20 -5))

(defmethod glut:keyboard ((w energy-based-window) key x y)
  (declare (ignore x y))
  ;(format t "key: ~A~%" key)
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\q (glut:destroy-current-window))
    (#\r (glut:display w))))



		 
    

  
  
    




