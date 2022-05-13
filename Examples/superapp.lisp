(defpackage #:clim-demo.app
  (:use #:clim #:clim-lisp)
  (:export #:app-main))

(in-package #:clim-demo.app)

(define-application-frame superapp ()
  ()
  (:panes
   (int :application :scroll-bars :both)
   (app :application :scroll-bars :both))
  (:layouts
   (default (spacing (:thickness 30)
             (spacing (:thickness 30)
               (vertically () int app))))))

(defun app-main ()
  (let ((frame (make-application-frame 'superapp)))
    (values frame
            (clim-sys:make-process
             (lambda ()
               (run-frame-top-level frame))))))

(find-application-frame 'superapp)
