(in-package :clim-clx-fb)

;;; Clx-Fb-MEDIUM class

(defclass clx-fb-medium (render-medium-mixin basic-medium)
  ((buffering :allocation :class
              :initform nil
              :accessor medium-buffering-output-p)))

(defmethod medium-finish-output :before ((medium clx-fb-medium))
  (when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output mirror)))

(defmethod medium-force-output :before ((medium clx-fb-medium))
  (when-let ((mirror (medium-drawable medium)))
    (%mirror-force-output mirror)))
