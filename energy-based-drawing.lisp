(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
;(require 'anaphora)
;(require 'cl-ftgl)

(declaim (optimize (debug 3)))

(defparameter *debug* nil)

;(defparameter data '((1 2 3) (2 1 3 4) (3 2 1) (4 2 5 6) (5 4 6) (6 4 5)))
(defparameter data '((1 2 3 4 5 6) (2 1) (3 1) (4 1) (5 1) (6 1)))
;(defparameter data '((1 6) (2) (3) (4) (5) (6 1)))

(defclass energy-based-drawer () 
  ((data :initarg :data :accessor data)
   (dimension :initarg :dimension :accessor dimension)
   (is-stable :initarg :is-stable :accessor is-stable)
   (font :accessor font-of)
   (file :reader file-of :initarg :file))
  (:default-initargs :data nil :dimension 2 :is-stable nil :file "example.otf"))

(defun is-debug ()
  *debug*)

(defun toggle-debug ()
  (setf *debug* (not (is-debug))))


(defmethod initialize-instance :after ((drawer energy-based-drawer) &rest other)
  (declare (ignore other))
  #|(setf (font-of drawer)
	(anaphora:aprog1 (cl-ftgl:create-buffer-font (file-of drawer))
	  (cl-ftgl:set-font-face-size drawer 80 72)
	  (cl-ftgl:set-font-char-map drawer :unicode)))|#
  (let ((data (data drawer)))
    (setf (data drawer) (make-hash-table))
    (mapcar (lambda (vertex)
	      (let ((key (car vertex)))
		(setf (gethash key (data drawer)) (make-instance 'vertex-data :vertex vertex :coordinates (list (+ (random 0.01) (* key 5)) (+ (random 0.01) (* key 5)))))
		(when (is-debug)
		  (format t "initial vertex ~A coordinates: ~A~%" key (coordinates (gethash key (data drawer)))))))
	    data)))


(defclass vertex-data ()
  ((vertex :initarg :vertex :accessor vertex)
   (velocity :initarg :velocity :accessor velocity)
   (coordinates :initarg :coordinates :accessor coordinates))
  (:default-initargs :velocity '(0 0) :coordinates (list (random 1.0) (random 1.0))))

(defun add-force (lhs-force rhs-force)
  (mapcar (lambda (x y) (+ x y)) lhs-force rhs-force))

(defun distance-base (lhs-points rhs-points)
  "Returns the distance between two points
This is flexible to handle 2 or 3 dimensions  "
  (sqrt (apply '+ (mapcar (lambda (c1 c2)
			    (expt (- c2 c1) 2))
			  lhs-points rhs-points))))

(defun recalibrate (lhs-points rhs-points)
  "Modifies very slightly two points to ensure they don't
have a null distance between them." 
  (labels ((randomize (points)
	     (maplist (lambda (c)
		       (setf (car c) (+ (random 0.0001) (car c))))
		      points)))
    (do ((lhs (randomize lhs-points) (randomize lhs-points))
	 (rhs (randomize rhs-points) (randomize rhs-points)))
	((not (eql (distance-base lhs rhs) 0))))))

(defun distance (lhs-points rhs-points)
  "Returns the distance between two points
This is flexible to handle 2 or 3 dimensions  "
  (let ((dist (sqrt (apply '+ (mapcar (lambda (c1 c2)
					(expt (- c2 c1) 2))
				      lhs-points rhs-points)))))
    (when (equal dist 0)
      (setf dist (recalibrate lhs-points rhs-points)))
    dist))

(defparameter *spring-length* 10)
(defparameter *spring-stiffness* 5000)


(defun hooke-attraction-for-component (distance)
  (* (- distance *spring-length*) *spring-stiffness*))


(defun hooke-attraction (lhs-coord rhs-coord)
  (let* ((distance (distance lhs-coord rhs-coord)))
    (mapcar (lambda (c1 c2)
	      (* (- c2 c1) (/ distance) (hooke-attraction-for-component distance)))
	    lhs-coord rhs-coord)))

;(hooke-attraction '(0 0) '(0.41931432 0.41931432))

(defparameter *coulomb-constant* 20000000)
(defun coulomb-repulsion (lhs-coord rhs-coord)
  (let* ((distance (distance lhs-coord rhs-coord))
	 (coulomb-repulsion (/ *coulomb-constant* (expt distance 2))))
    (mapcar (lambda (c1 c2)
	      (* (- c1 c2) (/ distance) coulomb-repulsion))
	    lhs-coord rhs-coord)))

;(coulomb-repulsion '(0.24730462 0.20116545) '(0.45538783 0.8761474))

(defparameter *timestep* 0.001)
(defparameter *damping* 0.5)
(defparameter *mass* 1)

(defun next-step (drawer)
  (when (is-debug)
    (format t "next-step~%"))
  (unless (is-stable drawer)
    (let ((total-kinetic-energy 0)
	  (new-velocities (make-hash-table))
	  (new-coordinates (make-hash-table)))
      ;(format t "process next-step~%")
      (maphash (lambda (key val)
		 (let ((net-force (Make-list (dimension drawer) :initial-element 0))
		       (coulomb-repulsion (make-list (dimension drawer) :initial-element 0))
		       (hooke-attraction (make-list (dimension drawer) :initial-element 0)))
		   
		   ;; for each other node.
		   (maphash (lambda (other-key other-val)
			      (unless (eq key other-key)
				(let ((c-r (coulomb-repulsion (coordinates val) (coordinates other-val))))
				  (when (is-debug)
				    (format t "  coulomb-repulsion from ~A to ~A: ~A~%" key other-key c-r))
				  (setf coulomb-repulsion (add-force coulomb-repulsion c-r)))))
			    (data drawer))
		   (when (is-debug)
		     (format t " vertex ~A coulomb-repulsion net-force: ~A~%" key coulomb-repulsion))
		   ;; for each spring connected to this node.
		   (mapcar (lambda (remote-vertex-label) 
			     (when (is-debug)
			       (format t "  hooke-attraction from ~A to ~A:~%" key remote-vertex-label))
			     (let ((h-a (hooke-attraction (coordinates val) (coordinates (gethash remote-vertex-label (data drawer))))))
			       (when (is-debug)
				 (format t "   hooke-attraction result: ~A~%" h-a))
			       (setf hooke-attraction (add-force hooke-attraction h-a))))
			   (cdr (vertex val)))
		   (when (is-debug)
		     (format t " vertex ~A hooke-attraction net-force: ~A~%" key hooke-attraction))
		   (setf net-force (add-force coulomb-repulsion hooke-attraction))
		   (when (is-debug)
		     (format t " vertex ~A net-force: ~A~%" key net-force))
		   
		   (setf (gethash key new-velocities) (mapcar (lambda (v n-f)
								(* (+ v (* *timestep* n-f)) *damping*))
							      (velocity val) net-force))
		   (setf (gethash key new-coordinates) (mapcar (lambda (c v)
								 (+ c (min 1.0 (* *timestep* v))))
							       (coordinates val) (gethash key new-velocities)))
		   (setf total-kinetic-energy (+ total-kinetic-energy (* *mass* (sqrt (apply '+ (mapcar (lambda (c) 
													  (expt c 2))
													(gethash key new-velocities)))))))))
	       (data drawer))
      (maphash (lambda (k v)
		 (setf (velocity (gethash k (data drawer))) v))
	       new-velocities)
      (maphash (lambda (k v)
		 (when (is-debug)
		   (format t " vertex ~A new-coordinates: ~A~%" k v))
		 (setf (coordinates (gethash k (data drawer))) v))
	       new-coordinates))))
;;       (when (< total-kinetic-energy 1)
;; 	(setf (is-stable drawer) t)))))

(defun draw (drawer)
  (next-step drawer)
  (maphash (lambda (k v)
	     (when (is-debug)
	       (format t "vertext ~A coordinates: ~A~%" k (coordinates v)))
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
  (when (is-debug)
    (format t "draw-one-line x1=~A y1=~A x2=~A y2=~A~%" x1 y1 x2 y2))
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
  (Glut:Post-redisplay))

(defmethod glut:reshape ((w energy-based-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((width (/ width 4))
	(height (/ height 4)))
    (glu:ortho-2d (- width) width (- height) height)))

(defmethod glut:keyboard ((w energy-based-window) key x y)
  (declare (ignore x y))
  ;(format t "key: ~A~%" key)
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\q (glut:destroy-current-window))
    (#\d (toggle-debug))
    (#\s (setf (is-stable (drawer w)) (not (is-stable (drawer w)))))
    (#\r (glut:display w))))



		 
    

  
  
    




