;; -*- mode:Lisp -*-
;#-:asdf+ (error "ASDF+ is required.")

(defpackage #:ebd-system
  (:use #:cl #:asdf)
  (:export #:start))

(in-package #:ebd-system)

(defsystem :ebd
    :depends-on (:cl-opengl :cl-glu :cl-glut :cffi)
    :components ((:file "package")
		 (:file "energy-based-drawing" :depends-on ("package"))))

