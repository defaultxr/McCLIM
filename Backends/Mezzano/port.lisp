(in-package :clim-mezzano)

(defvar *port* NIL)

(defmacro with-port-lock ((port) &body body)
  (let ((thunk (gensym "PORT-LOCK-THUNK"))
        (lock (gensym)))
    `(flet ((,thunk () ,@body))
       (let ((,lock (mezzano-port-lock ,port)))
         (if (mezzano.supervisor:mutex-held-p ,lock)
             (,thunk)
             (mezzano.supervisor:with-mutex (,lock) (,thunk)))))))

;;======================================================================
;; Define pointer class
;;======================================================================

(defclass mezzano-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0 :accessor pointer-x)
   (y :initform 0 :accessor pointer-y)))

(defmethod pointer-position ((pointer mezzano-pointer))
  (values (pointer-x pointer) (pointer-y pointer)))

(defmethod synthesize-pointer-motion-event ((pointer mezzano-pointer))
  (let ((port (port pointer)))
    (with-port-lock (port)
      (make-instance 'pointer-motion-event
                     :sheet (pointer-sheet pointer)
                     :pointer pointer
                     :graft-x (pointer-x pointer)
                     :graft-y (pointer-y pointer)
                     :button 0
                     :modifier-state 0))))

;;======================================================================
;; Define port class
;;======================================================================

;;
;; All mezzano events are piped through a single fifo (mez-fifo) which
;; is read by the event thread (initialize-event-thread). step-event-loop
;; translates mezzano events to mcclim events using mez-event->mcclim-event,
;; which in turn uses mez-window->sheet to figure out which mcclim sheet
;; corresponds to the mezzano window which received the  event. The mcclim
;; events are sent to the appropriate sheet using distribute-event
;;

(defclass mezzano-port (mcclim-render-internals::render-port-mixin
                        ; standard-event-port-mixin
                        standard-port)
  ((pointer            :reader   port-pointer)
   (window             :accessor mezzano-port-window)
   (display-thread     :accessor mezzano-display-thread)
   (event-thread       :accessor mezzano-event-thread)
   (cursor-table       :accessor mezzano-cursor-table)
   (mez-window->sheet  :initform (make-hash-table :test #'eq))
   (mez-window->mirror :initform (make-hash-table :test #'eq))
   (mez-fifo           :reader   mezzano-mez-fifo)
   (lock               :reader   mezzano-port-lock)))

(defun parse-mezzano-server-path (path)
  (list :mezzano :host       :mezzano
		 :display-id 0
		 :screen-id  0
		 :protocol   :native))

(setf (get :mezzano :port-type) 'mezzano-port)
(setf (get :mezzano :server-path-parser) 'parse-mezzano-server-path)

;; this sucks! required because some mezzano events generate multiple mcclim events.
(defvar *outstanding-events* nil)

(defun initialize-event-thread (port)
  (when clim-sys:*multiprocessing-p*
    (mos:make-thread
     (lambda ()
       (let ((*terminal-io* (make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                           :title "McCLIM event loop console")))
         (loop (with-simple-restart
		   (restart-event-loop "Restart CLIM's event loop.")
                 (let ((*outstanding-events* '()))
                   (loop
                      (step-event-loop port)))))))
     :name "McCLIM Events")))

(defun step-event-loop (port)
  (process-next-event port))

(defmethod initialize-instance :after ((port mezzano-port) &rest args &key host display-id screen-id protocol)
  (declare (ignore args))
  ;; TODO why are these initargs passed?
  (declare (ignore host display-id screen-id protocol))
  (setf (slot-value port 'mez-fifo) (mos:make-mailbox :name `(mez-fifo ,port)
                                                      :capacity 50))
  (setf (slot-value port 'lock) (mezzano.supervisor:make-mutex port))
  (setf *port* port
        (slot-value port 'pointer) (make-instance 'mezzano-pointer :port port)
        (mezzano-port-window port) (mos:current-framebuffer))
  (push (apply #'make-instance 'mezzano-frame-manager
               :port port
					; TODO (cdr (port-server-path port))
	       '())
	(slot-value port 'frame-managers))
  (make-graft port)
  (clim-extensions:port-all-font-families port)
  (setf (mezzano-event-thread port) (initialize-event-thread port)))

(defmethod destroy-port :before ((port mezzano-port))
  ;; TODO - there's more to clean up here:
  ;; close any mez-windows/mez-frames
  ;; destroy the associated frame-manager
  (bt:destroy-thread (mezzano-event-thread port)))

(defun port-window-sheet (port mez-window)
  (with-port-lock (port)
    (gethash mez-window (slot-value port 'mez-window->sheet))))

(defun port-window-mirror (port mez-window)
  (with-port-lock (port)
    (gethash mez-window (slot-value port 'mez-window->mirror))))

(defmethod port-enable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  )

(defmethod port-frame-keyboard-input-focus ((port mezzano-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port mezzano-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defun create-mezzano-mirror (port sheet title width height top-border
                              &key (close-button-p t) (resizablep t))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (setf x (max 0 (round-coordinate x))
          y (max 0 (round-coordinate y))
          w (max 5 (round-coordinate w))
          h (max 5 (round-coordinate h)))
    (let* ((fwidth (+ width 2))
           (fheight (+ height 1 top-border))
           (mirror (make-instance 'mezzano-mirror))
           (fifo (mezzano-mez-fifo port))
           (window (mos:make-window fifo fwidth fheight))
           (surface (mos:window-buffer window))
           (frame (make-instance 'mos:frame
                                 :top top-border
                                 :framebuffer surface
                                 :title title
                                 :close-button-p close-button-p
                                 :resizablep resizablep
                                 :damage-function (mos:default-damage-function window)
                                 :set-cursor-function (mos:default-cursor-function window))))
      (setf (slot-value mirror 'mcclim-render-internals::dirty-region) nil
            (slot-value mirror 'fwidth) fwidth
            (slot-value mirror 'fheight) fheight
            (slot-value mirror 'dx) 1
            (slot-value mirror 'dy) top-border
            (slot-value mirror 'width) width
            (slot-value mirror 'height) height
            (slot-value mirror 'mez-pixels) (mos:surface-pixels surface)
            (slot-value mirror 'mez-window) window
            (slot-value mirror 'mez-frame) frame)
      (unless (and (zerop x) (zerop y))
        (mos:move-window window x y))
      (setf (gethash window (slot-value port 'mez-window->sheet)) sheet
            (gethash window (slot-value port 'mez-window->mirror)) mirror)
      mirror)))

(defmethod realize-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (%realize-mirror port sheet))

(defmethod %realize-mirror ((port mezzano-port) (sheet top-level-sheet-pane))
  (with-port-lock (port)
    (let* ((q (compose-space sheet))
           (frame (pane-frame sheet))
           (mirror (create-mezzano-mirror
                    port sheet
                    (frame-pretty-name frame)
                    (round-coordinate (space-requirement-width q))
                    (round-coordinate (space-requirement-height q))
                    19)))
      (setf (slot-value mirror 'top-levelp) t)
      mirror)))

(defmethod %realize-mirror ((port mezzano-port) (sheet unmanaged-top-level-sheet-pane))
  (with-port-lock (port)
    (create-mezzano-mirror port sheet "" 300 300 2
                           :close-button-p NIL
                           :resizablep NIL)))

(defmethod make-medium ((port mezzano-port) sheet)
  (make-instance 'mezzano-medium
                 :port port
                 ;; :graft (find-graft :port port)
                 :sheet sheet))

(defmethod make-graft ((port mezzano-port) &key (orientation :default) (units :device))
  (with-port-lock (port)
    (let* ((framebuffer (mos:current-framebuffer))
           (w (mos:framebuffer-width framebuffer))
           (h (mos:framebuffer-height framebuffer))
           (region (make-bounding-rectangle 0 0 w h)))
      (make-instance 'mezzano-graft
                     :port port
                     :region region
                     :mirror (mezzano-port-window port)
                     :orientation orientation
                     :units units))))

(defmethod graft ((port mezzano-port))
  (with-port-lock (port)
    (first (port-grafts port))))

(defmethod port-force-output ((port mezzano-port))
  (with-port-lock (port)
    (maphash (lambda (window mirror)
               (mcclim-render-internals::%mirror-force-output mirror)
               (image-mirror-to-mezzano mirror))
             (slot-value port 'mez-window->mirror))))

;; TODO: Implement WAIT-FUNCTION.
(defmethod process-next-event ((port mezzano-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (flet ((pop-event ()
           (when *outstanding-events*
             (return-from pop-event (pop *outstanding-events*)))
           (let ((mez-fifo (mezzano-mez-fifo port)))
             (mezzano.supervisor:event-wait-for
                 ((mezzano.sync:mailbox-receive-possible-event mez-fifo)
                  :timeout timeout)
               ;; Converting the event can ultimately produce no mcclim events,
               ;; so it gets done in the event-wait-for predicate to keep
               ;; the timeout working properly.
               (multiple-value-bind (event validp)
                   (mezzano.sync:mailbox-receive mez-fifo :wait-p nil)
                 (and validp
                      ;; The lock is held for exactly this form to synchronize
                      ;; with %REALIZE-MIRROR. Mezzano windows can be created
                      ;; in one thread, triggering an activation event, but
                      ;; the event thread could receive that event *before*
                      ;; the associated sheet was registered with the port!
                      ;; The activation event would then be dropped and the
                      ;; thing wouldn't be drawn initially.
                      (with-port-lock (port)
                        (let ((mcclim-events (mez-event->mcclim-event port event)))
                          (cond ((listp mcclim-events)
                                 (setf *outstanding-events* (rest mcclim-events))
                                 (first mcclim-events))
                                (mcclim-events))))))))))
    (let ((event (pop-event)))
      (cond (event
             ;; Not sure if this is correct. SYNTHESIZE-BOUNDARY-EVENTS seems to
             ;; maintain POINTER-SHEET except when it is NIL, in which case it
             ;; doesn't do anything and breaks all mouse input!
             ;;
             ;; -- is this still correct? --jd
             (let ((pointer (port-pointer port)))
               (when (and (typep event 'pointer-event)
                          (not (pointer-sheet pointer)))
                 (setf (pointer-sheet pointer) (event-sheet event))))
             (distribute-event port event)
             t)
            (t
             (values nil :timeout))))))

;;; Pixmap

(defclass mezzano-pixmap (image-pixmap-mixin permanent-medium-sheet-output-mixin)
  ())

(defmethod port-allocate-pixmap ((port mezzano-port) sheet width height)
  (make-instance 'mezzano-pixmap :sheet sheet :width width :height height :port port))

(defmethod port-deallocate-pixmap ((port mezzano-port) pixmap)
  (declare (ignore port pixmap)))

(defmethod set-sheet-pointer-cursor
    ((port mezzano-port) (sheet mirrored-sheet-mixin) cursor)
  ;; TODO - do we need to do anything here - or is the pointer cursor
  ;; completely managed by compositor?
  (unless (eq cursor :default)
    (debug-format "set-sheet-pointer-cursor ((port mezzano-port) (sheet mirrored-sheet-mixin) cursor)")
    (debug-format "    ~S ~S ~S" port sheet cursor)
    #+(or)
    (break))
  )

#+no (defmethod mcclim-render-internals::%set-image-region (mirror region)
  (debug-format "mcclim-render-internals::%set-image-region (mirror region)")
  (debug-format "    ~S ~S" mirror region))

;; TODO: Theses should show/hide the window, but that needs compositor support.
;; They're stubbed out because the listener requires them.

(defmethod port-enable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  nil)
