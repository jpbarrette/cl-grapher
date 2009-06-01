(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
;(require 'anaphora)
;(require 'cl-ftgl)
(glut:init)

(declaim (optimize (debug 3)))

(defparameter *debug* nil)

;(defparameter data '((1 2 3) (2 1 3 4) (3 2 1) (4 2 5 6) (5 4 6) (6 4 5)))
;(defparameter data '((1 2 3 4 5 6) (2 1) (3 1) (4 1) (5 1) (6 1) (7 8 9) (8 7 9) (9 8 7)))
;(defparameter data '((1 6) (2) (3) (4) (5) (6 1)))
(defparameter data '((1 2 3) 
		     (2 3 1) 
		     (3 2 1 4) 
		     (4 3 5 8) 
		     (5 4 6 7 8) 
		     (6 5 7)
		     (7 5 6 8 9) 
		     (8 5 7 9 4 14 17)
		     (9 7 8 10) 
		     (10 9 11 13)
		     (11 10 12)
		     (12 11 13)
		     (13 10 12)
		     (14 8 15 17)
		     (15 14 16)
		     (16 15)
		     (17 8 14 18)
		     (18 17 19 20)
		     (19 18)
		     (20 18)
		     (21 22 23) 
		     (22 21 23)
		     (23 21 22 24)
		     (24 23 25)
		     (25 24 26 27)
		     (26 25 27)
		     (27 25 26)))
		     

(defclass energy-based-drawer () 
  ((data :accessor data)
   (graph :initarg :graph :accessor graph)
   (dimension :initarg :dimension :accessor dimension)
   (is-stable :initarg :is-stable :accessor is-stable)
   (font :accessor font-of)
   (dragging :initarg :dragging :accessor dragging)
   (file :reader file-of :initarg :file)
   (vertice-list :initarg :vertice-list :accessor vertice-list))
  (:default-initargs :dimension 3 :is-stable nil :file "example.otf" :vertice-list nil :dragging nil))
 

;defmethod initialize-instance :after ((drawer energy-based-drawer) &key)
    
(defun is-debug ()
  *debug*)

(defun toggle-debug ()
  (setf *debug* (not (is-debug))))

#|(defmethod initialize-instance :after ((w energy-based-window) &key)
  ;(gl:shade-model :smooth)
;  (gl:enable :texture-2d)
;  (gl:enable :line-smooth)
;  (gl:enable :point-smooth)
;  (gl:enable :polygon-smooth)
;  (gl:enable :blend)
;  (gl:enable :depth-test)
;  (gl:hint :point-smooth-hint :dont-care)
;  (gl:hint :line-smooth-hint :dont-care)
;  (gl:hint :polygon-smooth-hint :dont-care)
;  (gl:hint :perspective-correction-hint :dont-care)
;  (gl:depth-mask 0)
;  (gl:blend-func :src-alpha :one-minus-src-alpha)
;  (gl:depth-func :equal))|#


(defun init-drawer (drawer)
  (setf (is-stable drawer) nil)
  (setf (data drawer) (make-hash-table))
  (mapcar (lambda (vertex)
	    (let ((key (car vertex)))
	      (setf (gethash key (data drawer)) (make-instance 'vertex-data :vertex vertex :coordinates (list (random 0.1) (random 0.1) 0)))
	      (when (is-debug)
		(format t "initial vertex ~A coordinates: ~A~%" key (coordinates (gethash key (data drawer)))))))
	  (graph drawer)))


(defmethod initialize-instance :after ((drawer energy-based-drawer) &rest other)
  (declare (ignore other))
  #|(setf (font-of drawer)
	(anaphora:aprog1 (cl-ftgl:create-buffer-font (file-of drawer))
	  (cl-ftgl:set-font-face-size drawer 80 72)
	  (cl-ftgl:set-font-char-map drawer :unicode)))|#
  (init-drawer drawer))



(defclass vertex-data ()
  ((vertex :initarg :vertex :accessor vertex)
   (velocity :initarg :velocity :accessor velocity)
   (coordinates :initarg :coordinates :accessor coordinates)
   (fixed :initarg :fixed :accessor fixed))
  (:default-initargs :velocity '(0 0 0) :coordinates (list (random 1.0) (random 1.0) 0.0) :fixed nil))

(defun add-force (lhs-force rhs-force)
  (mapcar (lambda (x y) (+ x y)) lhs-force rhs-force))

(defun distance-base (lhs-points rhs-points)
  "Returns the distance between two points
This is flexible to handle 2 or 3 dimensions  "
  (sqrt (apply '+ (mapcar (lambda (c1 c2)
			    (expt (- c2 c1) 2))
			  lhs-points rhs-points))))

(defun distance (lhs-points rhs-points)
  "Returns the distance between two points
This is flexible to handle 2 or 3 dimensions  "
  (let ((dist (max 0.0000001 (sqrt (apply '+ (mapcar (lambda (c1 c2)
						       (expt (- c2 c1) 2))
						     lhs-points rhs-points))))))
    dist))

(defparameter *spring-length* 10)
(defparameter *spring-stiffness* 500000)


(defun hooke-attraction-for-component (distance)
  (* (- distance *spring-length*) *spring-stiffness*))


(defun hooke-attraction (lhs-coord rhs-coord)
  (let* ((distance (distance lhs-coord rhs-coord)))
    (mapcar (lambda (c1 c2)
	      (* (- c2 c1) (/ distance) (hooke-attraction-for-component distance)))
	    lhs-coord rhs-coord)))

;(hooke-attraction '(0 0) '(0.41931432 0.41931432))

(defparameter *coulomb-constant* 200000000)
(defun coulomb-repulsion (lhs-coord rhs-coord)
  (let* ((distance (distance lhs-coord rhs-coord))
	 (coulomb-repulsion (/ *coulomb-constant* (expt distance 2))))
    (mapcar (lambda (c1 c2)
	      (* (- c1 c2) (/ distance) coulomb-repulsion))
	    lhs-coord rhs-coord)))

;(coulomb-repulsion '(0.24730462 0.20116545) '(0.45538783 0.8761474))

(defparameter *timestep* 0.0001)
(defparameter *damping* 0.8)
(defparameter *mass* 10)

(defun next-step (drawer)
  (when (is-debug)
    (format t "next-step~%"))
  (unless (is-stable drawer)
    (let ((total-kinetic-energy 0)
	  (new-velocities (make-hash-table))
	  (new-coordinates (make-hash-table)))
      ;(format t "process next-step~%")
      (maphash (lambda (key val)
		 (let ((net-force (make-list (dimension drawer) :initial-element 0))
		       (coulomb-repulsion (make-list (dimension drawer) :initial-element 0))
		       (hooke-attraction (make-list (dimension drawer) :initial-element 0)))
		   (when (not (fixed val))
		     ;; for each other node.
		     (maphash (lambda (other-key other-val)
				(unless (or (eq key other-key) (> (distance (coordinates val) (coordinates other-val)) (* *spring-length* 5)))
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
								   ;; Ensure we don't mode more than 0.1 away (negative/positive)
								   (+ c (max -3.0 (min 3.0 (* *timestep* v)))))
								 (coordinates val) (gethash key new-velocities)))
		     (setf total-kinetic-energy (+ total-kinetic-energy (* *mass* (sqrt (apply '+ (mapcar (lambda (c) 
													    (expt c 2))
													  (gethash key new-velocities))))))))))
	       (data drawer))
      (maphash (lambda (k v)
		 (setf (velocity (gethash k (data drawer))) v))
	       new-velocities)
      (maphash (lambda (k v)
		 (when (is-debug)
		   (format t " vertex ~A new-coordinates: ~A~%" k v))
		 (setf (coordinates (gethash k (data drawer))) v))
	       new-coordinates)
      (when (is-debug)
	(format t "total-kinetic-energy=~A~%" total-kinetic-energy))
      (when (< total-kinetic-energy 0.0000001)
 	(setf (is-stable drawer) t)))))

(defun draw (drawer)
  (next-step drawer)
  (gl:line-width 1)
  (setf (vertice-list drawer) (gl:gen-lists 1))
  (let ((qobj (glu::new-quadric))) 
    (gl:with-new-list ((vertice-list drawer) :compile)
      (glu::disk qobj 0 2 32 32))
    (glu::delete-quadric qobj))
  (maphash (lambda (k v)
	     (when (is-debug)
	       (format t "vertext ~A coordinates: ~A~%" k (coordinates v)))
	     (let ((x (car (coordinates v)))
		   (y (cadr (coordinates v))))
	       (draw-vertice drawer x y)
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

#|(defun make-circle ()
  (let ((qobj (glu:new-quadric)))
    (gl:with-pushed-matrix
      (gl:translate 0 0 0)
      (glu:quadric-draw-style qobj :fill)
      (glu:quadric-normals qobj :smooth)
      (glu::disk qobj 10 13 2 2))))|#

(defun draw-vertice (drawer x y) 
  (gl:with-pushed-matrix 
    (gl:translate x y 0)
    (gl:call-list (vertice-list drawer))))

#|(defun draw-vertice () 
  (let ((my-list (gl:gen-lists 1)) 
	(qobj (glu::new-quadric))) 
    (gl:with-new-list (my-list :compile) (gl:color 1 0 0) (glu::disk qobj 0 1.5 32 32))
    (gl:call-list my-list)))|#



(defmethod glut:display-window :before ((w energy-based-window))
  (gl:clear-color #x00ff #x00ff #x00ff 0)
  (gl:shade-model :smooth))

(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defmethod glut:display ((w energy-based-window))
  (gl:clear :color-buffer-bit)
  ; white for all lines
  (gl:color 0.2 0 0.1)
  ;; 
  (draw (drawer w))
  (gl:flush))

(defmethod glut:idle ((w energy-based-window))
  (next-step (drawer w))
  (glut:post-redisplay))

(defmethod glut:reshape ((w energy-based-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((width (/ width 4))
	(height (/ height 4)))
    (glu:ortho-2d (- width) width (- height) height))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((w energy-based-window) key x y)
  (declare (ignore x y))
  ;(format t "key: ~A~%" key)
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\q (glut:destroy-current-window))
    (#\d (toggle-debug))
    (#\s (setf (is-stable (drawer w)) (not (is-stable (drawer w)))))
    (#\r (init-drawer (drawer w)))))

(defun get-window-coordinates (p)
  (multiple-value-bind (x y)
      (apply #'glu:project p)
    (list (round x) (- (elt (gl:get-integer :viewport) 3) (round y)) 0)))

(defun find-vertice (drawer x y)
  (let ((nearest nil))
    (maphash (lambda (key val) 
	       (let* ((window-coordinates (get-window-coordinates (coordinates val)))
		      (distance (distance window-coordinates (list x y 0))))
		 (when (< distance 7)
		   (setf nearest key)
		   (format t "vertice ~A distance=~A~%" key distance))))
	     (data drawer))
    nearest))

(defun drag (drawer key x y)
  (when key
    (when (not (eq key (dragging drawer)))
      (setf (dragging drawer) key)
      (setf (fixed (gethash (dragging drawer) (data drawer))) t))
    (let ((val (gethash (dragging drawer) (data drawer))))
      (setf (coordinates val)
	    (get-object-coordinates x y)))))

(defun undrag (drawer)
  (when (dragging drawer)
    (setf (fixed (gethash (dragging drawer) (data drawer))) nil)
    (setf (dragging drawer) nil)))

(defmethod glut:mouse ((w energy-based-window) button state x y)
  (cond ((and (eq button :left-button) (eq state :up))
	 (undrag (drawer w)))
	((and (eq button :left-button) (eq state :down))
	 (drag (drawer w) (find-vertice (drawer w) x y) x y)
	 (glut:post-redisplay))))

(defun get-object-coordinates (x y)
  (multiple-value-bind (obj-x obj-y)
      (glu:un-project x (- (elt (gl:get-integer :viewport) 3) y) 0)
    (list (coerce obj-x 'single-float) (coerce obj-y 'single-float) 0.0)))

(defmethod glut:motion ((w energy-based-window) x y)
  (let ((drawer (drawer w)))
    (when (dragging drawer)
      (drag drawer (dragging drawer) x y)
      (glut:post-redisplay))))


  
  
    




