(in-package #:climi)

(defmacro with-output-to-drawing-stream ((stream backend destination &rest args) &body body)
  (with-gensyms (cont)
    `(flet ((,cont (,stream) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-output-to-drawing-stream (function ,cont) ,backend ,destination ,@args))))

(defmethod invoke-with-output-to-drawing-stream (continuation backend destination &rest args)
  (with-port (port backend)
    (apply #'invoke-with-output-to-drawing-stream continuation port destination args)))

(defmethod invoke-with-output-to-drawing-stream (continuation backend (window null) &rest args)
  (let* ((port (find-port :server-path backend))
         (window (apply #'open-window-stream :port port :input-buffer nil args)))
    (multiple-value-prog1 (funcall continuation window)
      (finish-output window))))
