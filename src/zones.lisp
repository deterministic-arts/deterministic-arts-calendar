
(in-package #:deterministic-arts.calendar.internals)

(defmethod print-object ((object calendar:zone) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (prin1 'calendar:zone stream)
    (write-char #\space stream)
    (prin1 (calendar:expression object) stream))
  object)

(defmethod calendar:zone ((object calendar:zone))
  object)

(defmethod calendar:zone ((object symbol))
  (resolve-zone object nil))

(defmethod calendar:zone ((object cons))
  (resolve-zone (car object) (cdr object)))

(defmethod calendar:zone ((object t))
  (error 'simple-type-error
         :datum object :expected-type 'calendar:zone
         :format-control "~S is not a supported ~S designator"
         :format-arguments (list object 'calendar:zone)))



(defclass fixed-offset-zone (calendar:zone)
  ((offset :type integer :initarg :offset)))

(defconstant fixed-zone-base-hash (sxhash 'fixed-offset-zone))

(defvar *fixed-offset-zone-cache-lock* (make-lock "Fixed-Offset Zone Table"))
(defvar *fixed-offset-zones* (make-weak-hash-table :test #'eql))

(defun make-fixed-offset-zone (offset)
  (let ((offset (etypecase offset ((integer #.(* -1 60 60 25) #.(* 60 60 25)) offset))))
    (with-lock-held (*fixed-offset-zone-cache-lock*)
      (or (gethash offset *fixed-offset-zones*)
          (setf (gethash offset *fixed-offset-zones*)
                (make-instance 'fixed-offset-zone :offset offset))))))

(defvar *utc-zone* (make-fixed-offset-zone 0))
(unless (boundp 'calendar:*zone*) (setf calendar:*zone* *utc-zone*))

(defmethod calendar:expression ((object fixed-offset-zone))
  (with-slots (offset) object
    (if (zerop offset) '(:utc) `(:fixed ,offset))))

(defmethod resolve-zone ((name (eql :utc)) &optional arguments)
  (if (null arguments) *utc-zone*
      (error "zone designator ~S does not accept arguments" name)))

(defmethod resolve-zone ((name (eql :fixed)) &optional arguments)
  (cond
    ((null arguments) *utc-zone*)
    ((typep arguments '(cons integer null)) (make-fixed-offset-zone (car arguments)))
    (t (error "invalid arguments ~S for zone designator ~S" arguments name))))

(defmethod calendar:zone ((object integer))
  (make-fixed-offset-zone object))

(defmethod compute-zone-offset (moment (zone fixed-offset-zone))
  (declare (ignore moment))
  (with-slots (offset) zone (values offset (and (zerop offset) "UTC") nil)))

(defmethod calendar:equal ((object1 fixed-offset-zone) (object2 fixed-offset-zone))
  (eql (slot-value object1 'offset) (slot-value object2 'offset)))

(defmethod calendar:hash ((object fixed-offset-zone))
  (logand most-positive-fixnum (+ (* 31 fixed-zone-base-hash) (slot-value object 'offset))))


