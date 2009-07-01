(in-package "CL-USER")

(require "COCOA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "OpenGL" :gl))

(defclass simple-gl-view (ns:ns-opengl-view)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self simple-gl-view) (rect :<NSR>ect))
  (#_glClearColor 0.0 0.0 0.0 0.0)
  (#_glClear #$GL_COLOR_BUFFER_BIT)
  (draw-stuff)
  (#_glFlush))

(defun draw-stuff ()
  (#_glColor3f 1.0 0.85 0.35)
  (#_glBegin #$GL_TRIANGLES)
   (#_glVertex3f 0.0 0.6 0.0)
   (#_glVertex3f -0.2 -0.3 0.0)
   (#_glVertex3f 0.2 -0.3 0.0)
  (#_glEnd))

(defun show-simple-gl ()
  (ns:with-ns-rect (frame 0 0 300 300)
    (let* ((w (make-instance 'ns:ns-window
			     :with-content-rect frame
			     :style-mask (logior #$NSTitledWindowMask
						 #$NSClosableWindowMask
						 #$NSMiniaturizableWindowMask)
			     :backing #$NSBackingStoreBuffered
			     :defer t))
	   (v (make-instance 'simple-gl-view)))
      (#/setContentView: w v)
      (#/release v)
      (#/center w)
      (#/orderFront: w nil))))

(show-simple-gl)