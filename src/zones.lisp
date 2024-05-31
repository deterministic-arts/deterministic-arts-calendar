
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



(defstruct (calendar:transition (:copier nil) (:conc-name transition-) (:predicate calendar:transitionp)
                                (:constructor make-transition (epoch-second timestamp offset-before offset-after)))
  (epoch-second 0 :type calendar:epoch-second :read-only t)
  (timestamp (error "missing") :type calendar:timestamp :read-only t)
  (offset-before 0 :type integer :read-only t)
  (offset-after 0 :type integer :read-only t))

(defmethod calendar:epoch-second ((object calendar:transition) &optional zone)
  (declare (ignore zone))
  (transition-epoch-second object))

(defmethod calendar:timestamp ((object calendar:transition))
  (transition-timestamp object))

(defmethod calendar:offset-before ((object calendar:transition))
  (transition-offset-before object))

(defmethod calendar:offset-after ((object calendar:transition))
  (transition-offset-before object))

(defun transition-duration ((object calendar:transition))
  (- (transition-offset-after object) (transition-offset-before object)))

(defun transition-gap-p (object)
  (< (transition-offset-before object) (transition-offset-after object)))

(defun transition-overlap-p (object)
  (> (transition-offset-before object) (transition-offset-after object)))
