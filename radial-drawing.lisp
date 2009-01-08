(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)

(declaim (optimize (debug 3)))

(defparameter data '((1 12 2 25) (2 3 4 5) (3 6) (6 7 8) (7) (8) (4) (5 9 11) (9 10) (11)
		     (25 26 29) (26 35 27) (27 28) (28) (29 30 32) (30 31) (32 33 34) (33) (34)
		     (12 13 14 15 16 17) (13 18) (14 20 21) (15) (16) (17 22 23) (18 19) (19) (20) (21) (22) (23 24) (24)))

(defclass radial-drawer () 
  ((data :initarg :data :accessor data)
   (nb-leaves-cache :initarg :nb-leaves-cache :accessor nb-leaves-cache))
  (:default-initargs :nb-leaves-cache (make-hash-table)))

(defmethod initialize-instance :after ((drawer radial-drawer) &rest other-vars)
  (declare (ignore other-vars))
  (build-nb-leaves drawer))

(defun build-nb-leaves (drawer)
  (labels ((b-nb-l (vertice-key) 
	     (let* ((vertice (assoc vertice-key (data drawer)))
		    (childs (cdr vertice))
		    (nb-leaves 0))
	       (if (null childs)
		   (incf nb-leaves)
		   (dolist (child childs)
		     (incf nb-leaves (b-nb-l child))))
	       (setf (gethash vertice-key (nb-leaves-cache drawer)) nb-leaves)
	       nb-leaves)))
    (b-nb-l (caar (data drawer)))))

(defun nb-leaves (drawer vertice-key)
  (gethash vertice-key (nb-leaves-cache drawer)))

(defun degrees (radians)
  (/ (* 180.0 radians) pi))

(defun radians (degrees)
  (/ (* 3.14159 degrees) 180.0))

(defun build-angle (current-radius next-radius)
  (- 90 (degrees (asin (/ current-radius next-radius)))))

(defun radius (drawer depth)
  (declare (ignore drawer))
  depth)


(defun calculate-angle (drawer vertice-key vertice-angle vertice-depth child-vertice-key)
  (let ((child-vertice-angle (min (/ (* (nb-leaves drawer child-vertice-key)
					vertice-angle)
				     (nb-leaves drawer vertice-key))
				  (build-angle (radius drawer vertice-depth) (radius drawer (1+ vertice-depth))))))
    child-vertice-angle))
  
(defun draw (drawer)
  (labels ((d (vertice-key current-depth vertice-angle start-angle x y)
	     (let* ((vertice (assoc vertice-key (data drawer)))
		    (vertice-key (car vertice))
		    (childs (cdr vertice))
		    (current-angle start-angle))
	       (dolist (child-vertice-key childs)
		 (let ((child-vertice-angle (calculate-angle drawer vertice-key vertice-angle current-depth child-vertice-key)))
		   (let* ((child-draw-angle (+ current-angle (/ child-vertice-angle 2)))
			  (child-x (* (cos (radians child-draw-angle)) (1+ current-depth)))
			  (child-y (* (sin (radians child-draw-angle)) (1+ current-depth))))
		     (draw-one-line x y child-x child-y)
		     (d child-vertice-key (1+ current-depth) child-vertice-angle current-angle child-x child-y))
		   (incf current-angle child-vertice-angle))))))
    (let* ((vertice (car (data drawer)))
	   (vertice-key (car vertice))
	   (childs (cdr vertice))
	   (current-angle 0))
      (dolist (child-vertice-key childs)
	(let ((child-vertice-angle (* 360
				      (/ (nb-leaves drawer child-vertice-key)
					 (nb-leaves drawer vertice-key)))))
	  (let* ((child-draw-angle (+ current-angle (/ child-vertice-angle 2)))
		 (x (cos (radians child-draw-angle)))
		 (y (sin (radians child-draw-angle))))
	    (draw-one-line 0 0 x y)
	    (d child-vertice-key 1 child-vertice-angle current-angle x y)
	    (incf current-angle child-vertice-angle)))))))
  

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


		 
    

  
  
    




