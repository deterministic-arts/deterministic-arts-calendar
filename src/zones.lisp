
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
  ((offset :type offset-number :initarg :offset :reader offset-number)))

(defconstant fixed-zone-base-hash (sxhash 'fixed-offset-zone))

(defvar *fixed-offset-zone-cache-lock* (make-lock "Fixed-Offset Zone Table"))

;;; XXX: trivial garbage does odd things on Clasp according to the source
;;;      code, unconditionally replacing the TEST function with EQ if any
;;;      kind of weakness is requested (for CCL, the TEST is replaced only
;;;      for weak keys -- that's better, though I'd still prefer an error
;;;      to be signalled if the TEST is not EQ if such a constraint exists
;;;      for a concrete implementation, as it does with CCL.)
(defvar *fixed-offset-zones* (make-weak-hash-table :weakness :value :test #'eql))

(defun make-fixed-offset-zone (offset)
  (let ((offset (etypecase offset ((integer #.(* -1 60 60 25) #.(* 60 60 25)) offset))))
    (with-lock-held (*fixed-offset-zone-cache-lock*)
      (or (gethash offset *fixed-offset-zones*)
          (setf (gethash offset *fixed-offset-zones*)
                (make-instance 'fixed-offset-zone :offset offset))))))

(defvar +utc-zone+ (make-fixed-offset-zone 0))
(unless (boundp 'calendar:*zone*) (setf calendar:*zone* +utc-zone+))

(defmethod calendar:expression ((object fixed-offset-zone))
  (with-slots (offset) object
    (if (zerop offset) '(:utc) `(:fixed ,offset))))

(defmethod resolve-zone ((name (eql :utc)) &optional arguments)
  (if (null arguments) +utc-zone+
      (error "zone designator ~S does not accept arguments" name)))

(defmethod resolve-zone ((name (eql :fixed)) &optional arguments)
  (cond
    ((null arguments) +utc-zone+)
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



(defgeneric make-transition (moment offset-before offset-after))

(defstruct (transition (:copier nil) (:conc-name transition-) (:predicate transitionp)
                       (:constructor make-transition-1 (epoch-second timestamp offset-before offset-after)))
  (epoch-second (error "missing") :type calendar:epoch-second :read-only t)
  (timestamp (error "missing") :type calendar:timestamp :read-only t)
  (offset-before (error "missing") :type offset-number :read-only t)
  (offset-after (error "missing") :type offset-number :read-only t))

(defun transition-instant (object)
  (make-instant (transition-epoch-second object)))

(defun transition-duration-seconds (object)
  (- (transition-offset-after object) (transition-offset-before object)))

(defun transition-timestamp-before (object)
  (transition-timestamp object))

(defun transition-timestamp-after (object)
  (calendar:add-seconds (transition-timestamp object) (transition-duration-seconds object)))

(defun gap-transition-p (object)
  (< (transition-offset-before object) (transition-offset-after object)))

(defun overlap-transition-p (object)
  (> (transition-offset-before object) (transition-offset-after object)))

(defun list-valid-transition-offsets (object)
  (if (gap-transition-p object) nil
      (list (transition-offset-before object) (transition-offset-after object))))

(defun valid-transition-offset-p (offset object)
  (and (not (gap-transition-p object))
       (or (eql offset (transition-offset-before object))
           (eql offset (transition-offset-after object)))))

(defmethod make-transition ((moment calendar:instant) offset-before offset-after)
  (if (not (zerop (instant-nanos moment)))
      (error "invalid transition time ~S (nanos should be zero)" moment)
      (let ((epoch-second (calendar:epoch-second moment)))
        (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second (- epoch-second offset-before))
          (let ((timestamp (make-timestamp (make-date year month day weekday) (make-time hour minute second 0))))
            (make-transition-1 epoch-second timestamp offset-before offset-after))))))

(defmethod make-transition ((moment calendar:timestamp) offset-before offset-after)
  (if (not (zerop (timestamp-nanos moment)))
      (error "invalid transition time ~S (nanos should be zero)" moment)
      (let ((epoch-second (+ offset-before (calendar:epoch-second moment nil))))
        (make-transition-1 epoch-second moment offset-before offset-after))))
