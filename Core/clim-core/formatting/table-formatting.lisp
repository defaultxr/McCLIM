;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 Alexey Dejneka <adejneka@comail.ru>
;;;  (c) copyright 2003 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Macros and functions for formatting tables and item lists.
;;;

;;; TODO:
;;;
;;; - Check types: RATIONAL, COORDINATE, REAL?
;;; - Check default values of unsupplied arguments.
;;; - Better error detection.
;;; -
;;; - Multiple columns:
;;; - - all columns are assumed to have the same width;
;;; - - all columns have the same number of rows; they should have the
;;;     same height.
;;; - :MOVE-CURSOR T support.
;;; - All types of widths, heights.
;;;   width, height too?
;;; - Item list formatting: what is :EQUALIZE-COLUMN-WIDTHS?!
;;;
;;; - We should think about inserting actual gutter into the output
;;;   records so that the bounding box of a row or a cell it its
;;;   logical dimension.

;;; - I would prefer if the INITIALIZE-INSTANCE would grok spacing and
;;;   all. Hmm, is that correct?

;;; The question araise if we need to support something like:
;;; (formatting-row ()
;;;   (with-output-as-presentation ()
;;;     (formatting-cell ())
;;;     (formatting-cell ())))

;;; Further: Should this table be somehow dynamic? That is when cell
;;; contents change also rerun the layout protocol? Or is that somehow
;;; covered by the incremental redisplay?

;;; Guess, we just do, it couldn't hurt.

(in-package #:clim-internals)

;;; Macro argument checking

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FORMAT-ITEMS uses FORMATTING-CELL, so we need this function at
  ;; compile-time.
  (defun %maybe-check-constant-keyword-arguments
      (record-type standard-record-type arguments valid-keywords)
    (when (and (constantp record-type)
               (eq (eval record-type) standard-record-type))
      (loop for (key nil) on arguments by #'cddr
            when (constantp key)
            do (let ((key (eval key)))
                 (unless (or (eq key :record-type)
                             (member key valid-keywords))
                   (with-current-source-form (arguments)
                     (error "~@<~S is not a valid initarg for ~S.~@:>"
                            key standard-record-type))))))))

;;; Helper macro for defining FORMATTING-* macros in the corresponding
;;; INVOKE-FORMATTING-* functions.
;;;
;;; ARGS is a list of argument specifications of the form
;;;
;;;   (name &key bindp default transform passp)
;;;
;;; where NAME specifies the name of the keyword argument in the
;;; generated function and the generated macro.
;;;
;;; BINDP controls whether NAME should be bound when BODY is evaluated
;;; even if the argument is just "passed through".
;;;
;;; DEFAULT specifies a default value for the argument. Defaulting is
;;; done in the generated function, not the macro.
;;;
;;; TRANSFORM is a form that computes a new value for the argument
;;; based on the current value (that is, the form can refer to NAME),
;;; for example normalizing the value.
;;;
;;; PASSP, which defaults to true, controls whether the argument is
;;; passed to the output record as an initarg.
(defmacro define-formatting-macro ((name standard-output-record-class)
                                   args
                                   ((stream-var record-var) &body body))
  (check-type stream-var symbol)
  (check-type record-var symbol)
  (let* ((invoke-name (alexandria:symbolicate '#:invoke- name))
         (keywords '())
         (keyword-arguments '())
         (keyword-arguments/defaults '())
         (transformed-keyword-arguments '())
         (remove-keyword-arguments '()))
    (mapc (lambda (arg)
            (destructuring-bind (name &key bindp
                                           (default nil defaultp)
                                           (transform nil transformp)
                                           (passp t))
                arg
              (let ((keyword (alexandria:make-keyword name)))
                (alexandria:appendf keywords (list keyword))
                (alexandria:appendf keyword-arguments (list name))
                (when bindp
                  (alexandria:appendf keyword-arguments/defaults
                                      (list name)))
                (when (or defaultp transformp)
                  (alexandria:appendf keyword-arguments/defaults
                                      (list `(,name ,default))))
                (when (and passp (or defaultp transformp))
                  (alexandria:appendf transformed-keyword-arguments
                                      `(,keyword ,(if transformp
                                                      transform
                                                      name))))
                (when (or defaultp transformp (not passp))
                  (alexandria:appendf remove-keyword-arguments (list keyword))))))
          args)
    `(progn
       (defun ,invoke-name (stream continuation
                            &rest args
                            &key (record-type ',standard-output-record-class)
                                 ,@keyword-arguments/defaults
                            &allow-other-keys)
         (let ((initargs (alexandria:remove-from-plist
                          args :record-type ,@remove-keyword-arguments)))
           (apply #'invoke-with-new-output-record
                  stream
                  (lambda (,stream-var ,record-var) ,@body)
                  record-type
                  ,@transformed-keyword-arguments
                  initargs)))

       (defmacro ,name ((&optional (stream t)
                         &rest args
                         &key (record-type '',standard-output-record-class)
                              ,@keyword-arguments
                         &allow-other-keys)
                        &body body)
         (declare (ignore ,@keyword-arguments))
         ;; If RECORD-TYPE is the default, make sure that all keyword
         ;; arguments with constant keys correspond to known initargs.
         (%maybe-check-constant-keyword-arguments
          record-type ',standard-output-record-class args ',keywords)
         (gen-invoke-trampoline
          ',invoke-name
          (list (stream-designator-symbol stream '*standard-output*))
          args
          body)))))

;;; Cell formatting

;;; STANDARD-CELL-OUTPUT-RECORD class
(defclass standard-cell-output-record (cell-output-record
                                       standard-sequence-output-record)
  ((align-x    :initarg :align-x    :reader cell-align-x)
   (align-y    :initarg :align-y    :reader cell-align-y)
   (min-width  :initarg :min-width  :reader cell-min-width)
   (min-height :initarg :min-height :reader cell-min-height))
  (:default-initargs
   :align-x :left :align-y :baseline :min-width 0 :min-height 0))

(define-formatting-macro (formatting-cell standard-cell-output-record)
    ((align-x :default :left)
     (align-y :default :baseline)
     (min-width :default 0
                :transform (parse-space stream min-width :horizontal))
     (min-height :default 0
                 :transform (parse-space stream min-height :vertical)))
  ((stream record)
   (declare (ignore record))
   (with-temporary-margins (stream :left '(:absolute 0))
     (letf (((stream-cursor-position stream) (values 0 0)))
       (funcall continuation stream)))))


;;; Generic block formatting
(defclass block-output-record-mixin ()
  ()
  (:documentation "The class representing one-dimensional blocks of cells."))

(defmethod replay-output-record ((bl block-output-record-mixin) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (when (null region)
    (setq region (or (pane-viewport-region stream) +everywhere+)))
  (with-drawing-options (stream :clipping-region region)
    (let (other-records)
      (map-over-output-records-overlapping-region
       #'(lambda (record)
           (if (cell-output-record-p record)
               (replay-output-record record stream region x-offset y-offset)
               (push record other-records)))
       bl region x-offset y-offset)
      (mapc #'(lambda (record)
                (replay-output-record record stream region x-offset y-offset))
            (nreverse other-records)))))

(defmethod map-over-block-cells (function (block block-output-record-mixin))
  ;; ### we need to do better -- yeah! how?
  (labels ((foo (row-record)
             (map-over-output-records
              (lambda (record)
                (if (cell-output-record-p record)
                    (funcall function record)
                    (foo record)))
              row-record)))
    (declare (dynamic-extent #'foo))
    (foo block)))


;;; Row formatting

;;; STANDARD-ROW-OUTPUT-RECORD class
(defclass standard-row-output-record (row-output-record
                                      block-output-record-mixin
                                      standard-sequence-output-record)
  ())

(defmethod map-over-row-cells (function
                               (row-record standard-row-output-record))
  (map-over-block-cells function row-record))

(define-formatting-macro (formatting-row standard-row-output-record)
    ()
  ((stream record)
   (declare (ignore record))
   (funcall continuation stream)))


;;; Column formatting

;;; STANDARD-COLUMN-OUTPUT-RECORD class
(defclass standard-column-output-record (column-output-record
                                         block-output-record-mixin
                                         standard-sequence-output-record)
  ())

(defmethod map-over-column-cells (function (column-record standard-column-output-record))
  (map-over-block-cells function column-record))

(define-formatting-macro (formatting-column standard-column-output-record)
    ()
  ((stream record)
   (declare (ignore record))
   (funcall continuation stream)))


;;; Table formatting

;;; STANDARD-TABLE-OUTPUT-RECORD class
(defclass standard-table-output-record (table-output-record
                                        standard-sequence-output-record)
  (;; standard slots
   (x-spacing :initarg :x-spacing)
   (y-spacing :initarg :y-spacing)
   (multiple-columns :initarg :multiple-columns)
   (multiple-columns-x-spacing :initarg :multiple-columns-x-spacing)
   (equalize-column-widths :initarg :equalize-column-widths)
   ;; book keeping -- communication from adjust-table-cells to
   ;; adjust-multiple-columns
   (widths)
   (heights)                            ;needed?
   (rows))
  (:default-initargs
   :multiple-columns nil
   :multiple-columns-x-spacing nil
   :equalize-column-widths nil))

(defmethod initialize-instance :after ((table standard-table-output-record)
                                       &rest initargs)
  (declare (ignore initargs))
  (when (null (slot-value table 'multiple-columns-x-spacing))
    (setf (slot-value table 'multiple-columns-x-spacing)
          (slot-value table 'x-spacing))))

(defmethod replay-output-record ((table standard-table-output-record) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (when (null region)
    (setq region (or (pane-viewport-region stream) +everywhere+)))
  (with-drawing-options (stream :clipping-region region)
    (let (other-records)
      (map-over-output-records-overlapping-region
       #'(lambda (record)
           (if (or (column-output-record-p record)
                   (row-output-record-p record))
               (replay-output-record record stream region x-offset y-offset)
               (push record other-records)))
       table region x-offset y-offset)
      (mapc #'(lambda (record)
                (replay-output-record record stream region x-offset y-offset))
            (nreverse other-records)))))

(define-formatting-macro (formatting-table standard-table-output-record)
    ((move-cursor :default t :passp nil)
     (x-spacing :transform (parse-space stream (or x-spacing #\Space) :horizontal))
     (y-spacing :transform (parse-space stream (or y-spacing
                                                   (stream-vertical-spacing stream))
                                        :vertical))
     (multiple-columns :bindp t)
     (multiple-columns-x-spacing :transform (if multiple-columns-x-spacing
                                                (parse-space stream multiple-columns-x-spacing :horizontal)
                                                x-spacing))
     (equalize-column-widths))
  ((stream table)
   (multiple-value-bind (cursor-old-x cursor-old-y)
       (stream-cursor-position stream)
     (with-output-recording-options (stream :record t :draw nil)
       (funcall continuation stream)
       (force-output stream))
     (setf (stream-cursor-position stream)
           (values cursor-old-x cursor-old-y))
     (with-output-recording-options (stream :record nil :draw nil)
       (adjust-table-cells table stream)
       (when multiple-columns (adjust-multiple-columns table stream))
       (tree-recompute-extent table))
     (replay table stream)
     (setf (stream-cursor-position stream)
           (if move-cursor
               (with-bounding-rectangle* (:x2 x2 :y2 y2) table
                 (values x2 y2))
               (values cursor-old-x cursor-old-y))))))

;;; Think about rewriting this using a common superclass for row and
;;; column records.

(defmethod map-over-table-elements
    (function (table-record standard-table-output-record) (type (eql :row)))
  (labels ((row-mapper (table-record)
             (map-over-output-records
              (lambda (record)
                (if (row-output-record-p record)
                    (funcall function record)
                    (row-mapper record)))
              table-record)))
    (declare (dynamic-extent #'row-mapper))
    (row-mapper table-record)))

(defmethod map-over-table-elements
    (function (table-record standard-table-output-record) (type (eql :column)))
  (labels ((col-mapper (table-record)
             (map-over-output-records
              (lambda (record)
                (if (column-output-record-p record)
                    (funcall function record)
                    (col-mapper record)))
              table-record)))
    (declare (dynamic-extent #'col-mapper))
    (col-mapper table-record)))

(defmethod map-over-table-elements (function
                                    (table-record standard-table-output-record)
                                    (type (eql :row-or-column)))
  (labels ((row-and-col-mapper (table-record)
             (map-over-output-records
              (lambda (record)
                (if (or (row-output-record-p record)
                        (column-output-record-p record))
                    (funcall function record)
                    (row-and-col-mapper record)))
              table-record)))
    (declare (dynamic-extent #'row-and-col-mapper))
    (row-and-col-mapper table-record)))


;;; Item list formatting

(defclass standard-item-list-output-record (item-list-output-record
                                            block-output-record-mixin
                                            standard-sequence-output-record)
  ((x-spacing :initarg :x-spacing)
   (y-spacing :initarg :y-spacing)
   (n-columns :initarg :n-columns)
   (n-rows :initarg :n-rows)
   (max-width :initarg :max-width)
   (max-height :initarg :max-height)
   (initial-spacing :initarg :initial-spacing)
   (row-wise :initarg :row-wise))
  (:default-initargs
   :n-columns nil :n-rows nil :max-width nil :max-height nil
   :initial-spacing nil :row-wise t))

(defmethod map-over-item-list-cells
    (function (item-list-record standard-item-list-output-record))
  (map-over-block-cells function item-list-record))

(define-formatting-macro (formatting-item-list standard-item-list-output-record)
    ((move-cursor :default t :passp nil)
     (x-spacing :transform (parse-space stream (or x-spacing #\Space) :horizontal))
     (y-spacing :transform (parse-space stream (or y-spacing
                                                   (stream-vertical-spacing stream))
                                        :vertical))
     (n-columns)
     (n-rows)
     (stream-width :passp nil)
     (stream-height :passp nil)
     (max-width)
     (max-height)
     (initial-spacing)
     (row-wise :default t))
  ((stream item-list)
   (multiple-value-bind (cursor-old-x cursor-old-y)
       (stream-cursor-position stream)
     (with-output-recording-options (stream :record t :draw nil)
       (funcall continuation stream)
       (force-output stream))
     (adjust-item-list-cells item-list stream)
     (setf (output-record-position item-list)
           (stream-cursor-position stream))
     (setf (stream-cursor-position stream)
           (if move-cursor
               (with-bounding-rectangle* (:x2 x2 :y2 y2) item-list
                 (values x2 y2)) 
               (values cursor-old-x cursor-old-y)))
     (replay item-list stream)
     item-list)))

(defun format-items (items &rest args
                           &key (stream *standard-output*)
                                printer presentation-type
                                cell-align-x cell-align-y
                           &allow-other-keys)
  (let ((printer (if printer
                     (if presentation-type
                         (lambda (item stream)
                           (with-output-as-presentation (stream item presentation-type)
                             (funcall printer item stream)))
                         printer)
                     (if presentation-type
                         (lambda (item stream)
                           (present item presentation-type :stream stream))
                         #'prin1)))
        (args (alexandria:remove-from-plist
               args :stream :printer :presentation-type
                    :cell-align-x :cell-align-y)))
    (apply #'invoke-formatting-item-list
           stream
           (lambda (stream)
             (map nil (lambda (item)
                        (formatting-cell (stream :align-x cell-align-x
                                                 :align-y cell-align-y)
                          (funcall printer item stream)))
                  items))
           args)))

;;; Helper function

(defun make-table-array (table-record)
  "Given a table record, creates an array of arrays of cells in row major
  order. Returns (array-of-cells number-of-rows number-of-columns)"
  (let* ((row-based (block
                        find-table-type
                      (map-over-table-elements
                       (lambda (thing)
                         (cond ((row-output-record-p thing)
                                (return-from find-table-type t))
                               ((column-output-record-p thing)
                                (return-from find-table-type nil))
                               (t
                                (error "Something is wrong."))))
                       table-record
                       :row-or-column)
                      ;; It's empty
                      (return-from make-table-array (values nil 0 0))))
         (rows (make-array (if row-based 1 0)
                           :adjustable t
                           :fill-pointer (if row-based
                                             0
                                             nil)
                           :initial-element nil))
         (number-of-columns 0))
    (if row-based
        (map-over-table-elements
         (lambda (row)
           (let ((row-array (make-array 4 :adjustable t :fill-pointer 0)))
             (map-over-row-cells (lambda (cell)
                                   (vector-push-extend cell row-array))
                                 row)
             (vector-push-extend row-array rows)
             (maxf number-of-columns (length row-array))))
         table-record
         :row)
        (let ((col-index 0))
          (map-over-table-elements
           (lambda (col)
             (let ((row-index 0))
               (map-over-column-cells
                (lambda (cell)
                  (when (>= row-index (length rows))
                    (adjust-array rows (1+ row-index) :initial-element nil))
                  (let ((row-array (aref rows row-index)))
                    (cond ((null row-array)
                           (setf row-array
                                 (make-array (1+ col-index)
                                             :adjustable t
                                             :initial-element nil))
                           (setf (aref rows row-index) row-array))
                          ((>= col-index (length row-array))
                           (adjust-array row-array (1+ col-index)
                                         :initial-element nil))
                          (t nil))
                    (setf (aref row-array col-index) cell))
                  (incf row-index))
                col))
             (incf col-index))
           table-record
           :column)
          (setq number-of-columns col-index)))
    (values rows (length rows) number-of-columns)))

(defmethod adjust-table-cells ((table-record standard-table-output-record)
                               stream)
  (with-slots (x-spacing y-spacing equalize-column-widths) table-record
    ;; Note: for the purpose of layout it is pretty much irrelevant if
    ;;       this is a table by rows or a table by columns
    ;;
    ;; Since we have :baseline vertical alignment (and no :char
    ;; horizontal alignment like in HTML), we always work from rows.
    (multiple-value-bind (rows nrows ncols)
        (make-table-array table-record)
      (unless rows
        (return-from adjust-table-cells nil))

      (let ((widthen  (make-array ncols :initial-element 0))
            (heights  (make-array nrows :initial-element 0))
            (ascents  (make-array nrows :initial-element 0))
            (descents (make-array nrows :initial-element 0)))
        ;; collect widthen, heights
        (loop for row across rows
              for i from 0 do
                (loop for cell across row
                      for j from 0 do
                        ;; we have cell at row i col j at hand.
                        ;; width:
                        (when cell
                          (with-bounding-rectangle* (:width width :height height) cell
                            (maxf (aref widthen j)
                                  (max width (cell-min-width cell)))
                            (maxf (aref heights i)
                                  (max height (cell-min-height cell)))
                            (when (eq (cell-align-y cell) :baseline)
                              (multiple-value-bind (baseline) (output-record-baseline cell)
                                (maxf (aref ascents i) baseline)
                                (maxf (aref descents i) (- height baseline))))))))

        ;; baseline aligned cells can force the row to be taller.
        (loop for i from 0 below nrows do
             (maxf (aref heights i) (+ (aref ascents i) (aref descents i))))

        (when (slot-value table-record 'equalize-column-widths)
          (setf widthen (make-array ncols :initial-element (reduce #'max widthen :initial-value 0))))

        (setf (slot-value table-record 'widths) widthen
              (slot-value table-record 'heights) heights
              (slot-value table-record 'rows) rows)

        ;; Finally just put the cells where they belong.

        (multiple-value-bind (cx cy) (stream-cursor-position stream)
          (loop for row across rows
             for y = cy then (+ y h y-spacing)
             for h across heights
             for ascent across ascents
             do
               (loop for cell across row
                  for x = cx then (+ x w x-spacing)
                  for w across widthen do
                    (when cell
                      (adjust-cell* cell x y w h ascent)))))))))

(defmethod adjust-multiple-columns ((table standard-table-output-record) stream)
  (with-slots (widths heights rows
                      multiple-columns multiple-columns-x-spacing x-spacing y-spacing)
      table
    (let* ((mcolumn-width
            ;; total width of a column of the "meta" table.
            (+ (reduce #'+ widths)
               (* (1- (length widths)) x-spacing)
               multiple-columns-x-spacing))
           (n-columns
            (max 1
                 (if (eq multiple-columns t)
                     (floor (+ (- (stream-text-margin stream)
                                  (stream-cursor-position stream))
                               multiple-columns-x-spacing)
                            (+ mcolumn-width multiple-columns-x-spacing))
                     multiple-columns)))
           (column-size (ceiling (length rows) n-columns)) )
      (let ((y 0) (dy 0))
        (loop for row across rows
           for h across heights
           for i from 0
           do
             (multiple-value-bind (ci ri) (floor i column-size)
               (when (zerop ri)
                 (setf dy (- y)))
               (let ((dx (* ci mcolumn-width)))
                 (loop for cell across row do
                      (multiple-value-bind (x y) (output-record-position cell)
                        (setf (output-record-position cell)
                              (values (+ x dx) (+ y dy))))))
               (incf y h)
               (incf y y-spacing)))) )))

(defmethod adjust-item-list-cells ((item-list standard-item-list-output-record)
                                   stream)
  (with-slots (x-spacing y-spacing initial-spacing row-wise) item-list
    ;;
    ;; What we do:
    ;;
    ;; First we collect the items, then we figure out the width of the
    ;; single initial column. While doing so we collect the heights.
    (let ((items nil)
          (width 0)
          (heights nil))
      ;;
      (map-over-item-list-cells (lambda (item)
                                  (push item items))
                                item-list)
      (setf items (reverse items))
      (setf heights (make-array (length items)))
      ;;
      (loop for item in items
         for i from 0
         do
           (with-bounding-rectangle* (x1 y1 x2 y2) item
             (maxf width (- x2 x1))
             (setf (aref heights i) (- y2 y1))))
      ;;
      ;; Now figure out the number of rows and the number of columns to
      ;; layout to.
      ;;
      (let ((stream-width (- (stream-text-margin stream) (stream-cursor-position stream)
                             (if initial-spacing
                                 x-spacing
                                 0)))
            (N (length items))
            (column-width
             (+ width x-spacing)))
        ;; ### note that the floors below are still not correct
        (multiple-value-bind (n-columns n-rows)
            (with-slots (n-columns n-rows max-width max-height) item-list
              (cond (n-columns
                     (values n-columns (ceiling N n-columns)))
                    (n-rows
                     (values (ceiling N n-rows) n-rows))
                    (max-width
                     (let ((n-columns (max 1 (floor (+ max-width x-spacing)
                                                    (+ column-width x-spacing)))))
                       (values n-columns (ceiling N n-columns))))
                    (max-height
                     ;; difficult
                     ;; ###
                     (let ((n-rows (max 1 (floor (+ max-height y-spacing)
                                                 (+ (reduce #'max heights :initial-value 0)
                                                    (* (1- (length heights)) y-spacing)
                                                    y-spacing)))))
                       (values (ceiling N n-rows) n-rows)))
                    (t
                     (let ((n-columns (max 1 (floor (+ stream-width x-spacing)
                                                    (+ column-width x-spacing)))))
                       (values n-columns (ceiling N n-columns))))))
          ;;
          (cond (row-wise
                 ;; Here is the catch: When this is row-wise, this not
                 ;; not so different from a table.
                 ;; ### do the cells put alongside expose baseline adjust?
                 (let ((y 0))
                   (loop for yi below n-rows
                      while items
                      do
                        (let ((h 0))
                          (loop for xi below n-columns
                             for x = (if initial-spacing (floor x-spacing 2) 0)
                             then (+ x width x-spacing)
                             while items
                             do
                               (let ((item (pop items)))
                                 (maxf h (bounding-rectangle-height item))
                                 (adjust-cell* item x y width
                                               (bounding-rectangle-height item)
                                               (output-record-baseline item))))
                          (incf y (+ h y-spacing))))))
                (t
                 ;; This is somewhat easier ...
                 (let (h)
                   (loop for xi below n-columns
                      for x = (if initial-spacing (floor x-spacing 2) 0)
                      then (+ x width x-spacing)
                      while items
                      do
                        (loop for yi below n-rows
                           for y = 0 then (+ y y-spacing h)
                           while items do
                             (let ((item (pop items)))
                               (setf h (bounding-rectangle-height item))
                               (adjust-cell* item x y width h (output-record-baseline item)))))))))))))

(defun adjust-cell* (cell x y w h ascent)
  (setf (output-record-position cell)
        (values
         (case (cell-align-x cell)
           ((nil :left)   x)            ;###
           (:center (+ x (/ (- w (bounding-rectangle-width cell)) 2)))
           (:right  (- (+ x w) (bounding-rectangle-width cell))))
         (case (cell-align-y cell)
           (:top    y)
           (:bottom (- (+ y h) (bounding-rectangle-height cell)))
           (:center (+ y (/ (- h (bounding-rectangle-height cell)) 2)))
           ((nil :baseline)
            (multiple-value-bind (baseline) (output-record-baseline cell)
              ;; make (+ y ascents) line up with (+ y1 b)
              ;; that is y+a = y1+b -> y1= y+a-b
              (+ y (- ascent baseline))))))))
