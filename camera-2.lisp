(defclass camera ()
  ((position :initarg :position :accessor cam-position)
   (target :initarg :target :accessor target)
   (up :initarg :up :accessor up)
   (field-of-view :initarg :fov :accessor fov :type double-float)
   (aspect :initarg :aspect :accessor aspect :type double-float)
   (near :initarg :near :accessor near :type double-float)
   (far :initarg :far :accessor far :type double-float))
  (:default-initargs :position '(0 0 100) :target '(0 0 0) :up '(0 1 0) :fov 60 :aspect nil :near 1 :far 400))



(defgeneric look-at (camera))
(defmethod look-at (camera)
  (apply #'glu:look-at (append (cam-position camera) (target camera) (up camera))))

(defgeneric update-aspect-ratio (camera width height))
(defmethod update-aspect-ratio (camera width height)
  (setf (aspect camera) (float (/ width height)))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective (fov camera) (aspect camera) (near camera) (far camera))
  (gl:matrix-mode :modelview))

(defgeneric zoom (camera units))
(defmethod zoom (camera units)
  (incf (nth 2 (cam-position camera)) (float units)))
  