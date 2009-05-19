(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)

(declaim (optimize (debug 3)))

(defparameter data '((1 12 2 25) (2 3 4 5) (3 6) (6 7 8) (7) (8) (4) (5 9 11) (9 10) (11)
		     (25 26 29) (26 35 27) (27 28) (28) (29 30 32) (30 31) (32 33 34) (33) (34)
		     (12 13 14 15 16 17) (13 18) (14 20 21) (15) (16) (17 22 23) (18 19) (19) (20) (21) (22) (23 24) (24)))

(defclass energy-based-drawer () 
  ((data :initarg :data :accessor data)
   (dimension :initarg :dimensiohn :accessor dimension))
  (:default-initargs :data (make-hash-table) :dimension 2))

(defmethod initialize-instance :after ((drawer energy-based-drawer) &key data)
  (mapcar (lambda (vertex)
	    (setf (gethash (car vertex) (data drawer)) (make-instance 'vertex-data :vertex vertex)))
	  data))


(defclass vertex-data ()
  ((vertex :initarg :vertex :accessor vertex)
   (velocity :initarg :velocity :accessor velocity)
   (coordinates :initarg :coordinates :accessor coordinates)
   (is-stable :initarg :is-stable :accessor is-stable))
  (:default-initargs :velocity '(0 0) :coordinates (list (random 0.1) (random 0.1)) :is-stable nil))

(defun add-force (lhs-force rhs-force)
  (mapcar (lambda (x y) (+ x y)) lhs-force rhs-force))

(defun distance (lhs-points rhs-points)
  "Returns the distance between two points
This is flexible to handle 2 or 3 dimensions  "
  (sqrt (apply '+ (mapcar (lambda (c1 c2)
			    (expt (- c2 c1) 2))
			  lhs-points rhs-points))))

(defparameter *spring-length* 1)
(defparameter *spring-stiffness* 1)


(defun hooke-attraction-for-component (c1 c2 distance)
  (* (- distance *spring-length*) *spring-stiffness* (- c2 c1) (/ distance)))


(defun hooke-attraction (vertex other-vertex)
  (let* ((lhs-coord (coordinates vertex))
	 (rhs-coord (coordinates other-vertex))
	 (distance (distance lhs-coord rhs-coord)))
    (mapcar (lambda (c1 c2)
	      (* (- c2 c1) (/ distance) (hooke-attraction-for-component c1 c2 distance)))
	    lhs-coord rhs-coord)))

(defparameter *coulomb-constant* 1)
(defun coulomb-repulsion (vertex other-vertex)
  (let* ((lhs-coord (coordinates vertex))
	 (rhs-coord (coordinates other-vertex))
	 (distance (distance lhs-coord rhs-coord))
	 (coulomb-repulsion (/ *coulomb-constant* (expt distance 2))))
    (mapcar (lambda (c1 c2)
	      (* (- c2 c1) (/ distance) coulomb-repulsion))
	    lhs-coord rhs-coord)))

(defun next-step (drawer)
  (unless (is-stable drawer)
    (let ((total-kinetic-energy 0))
      (maphash (lambda (key val)
		 (let ((net-force (make-list (dimension drawer) :initial-element 0)))
		   ;; for each other node.
		   (maphash (lambda (other-key other-val)
			      (unless (eq key other-key)
				(add-force net-force (coulomb-repulsion val other-val))))
			    (data drawer))
		   ;; for each spring connected to this node.
		   (mapcar (lambda (remote-vertex-label) 
			     (add-force net-force (hooke-attraction val (gethash remote-vertex-label (data drawer)))))
			   (cdr (vertex val)))))
	       (data drawer))
      (incf total-kinetic-energy))))

;; (defclass binary-tree-window (glut:window) 
;;   ((drawer :accessor drawer :initarg :drawer))
;;   (:default-initargs 
;;    :width 400 :height 400 :pos-x 100 :pos-y 100
;;    :mode '(:single :rgb) :title "chapter.2.5.lisp"))

;; (defmethod glut:display-window :before ((w binary-tree-window))
;;   (gl:clear-color #x00ff #x00ff #x00ff 0)
;;   (gl:shade-model :flat))

;; (defun draw-one-line (x1 y1 x2 y2)
;;   (gl:with-primitives :lines
;;     (gl:vertex x1 y1)
;;     (gl:vertex x2 y2)))

;; (defmethod glut:display ((w binary-tree-window))
;;   (gl:clear :color-buffer-bit)
;;   ; white for all lines
;;   (gl:color 0 0 0)
;;   ;; 
;;   (draw (drawer w))
;;   (gl:flush))

;; (defmethod glut:reshape ((w binary-tree-window) width height) ;
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   (glu:ortho-2d -10 20 20 -5))

;; (defmethod glut:keyboard ((w binary-tree-window) key x y)
;;   (declare (ignore x y))
;;   (case key
;;     (#\Esc (glut:destroy-current-window)))) ;whi



;; (defparameter drawer (make-instance 'radial-drawer :data data))
;; (glut:display-window (make-instance 'binary-tree-window :drawer drawer))

		 
    

  
  
    




