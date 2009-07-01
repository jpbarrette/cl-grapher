
(defclass camera ()
  ((cam-x :initarg :cam-x :accessor cam-x)
   (cam-y :initarg :cam-y :accessor cam-y)
   (cam-z :initarg :cam-z :accessor cam-z)
   (near :initarg :near :accessor near)
   (far :initarg :far :accessor far)
   (fovy :initarg :fovy :accessor fovy)
   (ratio :initarg :ratio :accessor cam-ratio))
  (:default-initargs :cam-x 0 :cam-y 0 :cam-z 200 :fovy 60 :near 1 :far 300))

(defgeneric zoom (camera &key unit))
(defmethod zoom (camera &key (unit 5))
  (incf (cam-z camera) unit)
  (glut:post-redisplay))
  
(defgeneric perspective (camera width height))
(defmethod perspective (camera width height)
  (setf (cam-ratio camera) (/ width height))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (with-slots (cam-x cam-y cam-z near far fovy ratio)
      camera
    (glu:perspective fovy ratio near far))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defgeneric look-at (camera))
(defmethod look-at (camera)
  (gl:load-identity) ; clear the matrix
  (with-slots (cam-x cam-y cam-z near far fovy ratio)
      camera
    (glu:look-at cam-x cam-y cam-z 0 0 0 0 1 0)))
