#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Date and time library
  Copyright (c) 2023 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package #:darts.lib.calendar-internals)

(defstruct (calendar:instant (:copier nil) (:conc-name instant-) (:predicate calendar:instantp)
                             (:constructor make-instant (epoch-second &optional (nanos 0))))
  (epoch-second 0 :type calendar:epoch-second :read-only t)
  (nanos 0 :type (integer 0 999999999) :read-only t))

(defstruct (calendar:date (:copier nil) (:conc-name date-) (:predicate calendar:datep)
                          (:constructor make-date (year month day &optional (%weekday lazyday))))
  (year 0 :type year :read-only t)
  (month 1 :type (integer 1 12) :read-only t)
  (day 1 :type (integer 1 31) :read-only t)
  (%weekday lazyday :type (integer 0 7)))

(defstruct (calendar:time (:copier nil) (:conc-name time-) (:predicate calendar:timep)
                          (:constructor make-time (hour minute &optional (second 0) (nanos 0))))
  (hour 0 :type (integer 0 23) :read-only t)
  (minute 0 :type (integer 0 59) :read-only t)
  (second 0 :type (integer 0 59) :read-only t)
  (nanos 0 :type (integer 0 999999999) :read-only t))

(defstruct (calendar:timestamp (:copier nil) (:conc-name timestamp-) (:predicate calendar:timestampp)
                               (:constructor make-timestamp (date time)))
  (date (error "missing") :type calendar:date :read-only t)
  (time (error "missing") :type calendar:time :read-only t))

(defstruct (calendar:duration (:copier nil) (:conc-name duration-) (:predicate calendar:durationp)
                              (:constructor make-duration (seconds nanos)))
  (seconds 0 :type calendar:epoch-second :read-only t)
  (nanos 0 :type (integer 0 999999999) :read-only t))

#+SBCL
(declaim (sb-ext:freeze-type calendar:date calendar:time calendar:timestamp
                             calendar:instant calendar:duration))

(defun date-weekday (object)
  (let ((value (date-%weekday object)))
    (if (not (eql value lazyday)) value
        (setf (date-%weekday object)
              (compute-weekday (date-year object) (date-month object) (date-day object))))))

(defun timestamp-year (object)
  (date-year (timestamp-date object)))

(defun timestamp-month (object)
  (date-month (timestamp-date object)))

(defun timestamp-day (object)
  (date-day (timestamp-date object)))

(defun timestamp-weekday (object)
  (date-weekday (timestamp-date object)))

(defun timestamp-hour (object)
  (time-hour (timestamp-time object)))

(defun timestamp-minute (object)
  (time-minute (timestamp-time object)))

(defun timestamp-second (object)
  (time-second (timestamp-time object)))

(defun timestamp-nanos (object)
  (time-nanos (timestamp-time object)))

(defun calendar:duration-plusp (object)
  (or (plusp (duration-seconds object))
      (and (zerop (duration-seconds object))
           (plusp (duration-nanos object)))))

(defun calendar:duration-minusp (object)
  (minusp (duration-seconds object)))

(defun calendar:duration-zerop (object)
  (and (zerop (duration-seconds object))
       (zerop (duration-nanos object))))

(defun calendar:make-date (year month day)
  (let ((year (check-year year))
        (month (check-month month)))
    (make-date year month (check-day-of-month day year month))))

(defun calendar:make-time (hour minute &optional (second 0) (nanos 0))
  (make-time (check-hour hour) (check-minute minute) (check-second second)
             (check-nanos nanos)))

(defun calendar:make-timestamp (year month day &optional (hour 0) (minute 0) (second 0) (nanos 0))
  (make-timestamp (calendar:make-date year month day)
                  (calendar:make-time hour minute second nanos)))

(defun calendar:make-instant (epoch-second &optional (nanos 0))
  (make-instant (check-epoch-second epoch-second)
                (check-nanos nanos)))

(defun calendar:make-duration (seconds &optional (nanos 0))
  (multiple-value-bind (adjustment nanos) (floor nanos 1000000000)
    (make-duration (check-duration-seconds (+ seconds adjustment))
                   nanos)))

(defmethod calendar:copy ((object calendar:date) &key year month day)
  (if (not (or year month day)) object
      (calendar:make-date (or year (date-year object))
                          (or month (date-month object))
                          (or day (date-day object)))))

(defmethod calendar:copy ((object calendar:time) &key hour minute second nanos)
  (if (not (or hour minute second nanos)) object
      (calendar:make-time (or hour (time-hour object))
                          (or minute (time-minute object))
                          (or second (time-second object))
                          (or nanos (time-nanos object)))))

(defmethod calendar:copy ((object calendar:timestamp)
                          &key date time year month day hour minute second nanos)
  (let ((overrides-date (or year month day))
        (overrides-time (or hour minute second nanos)))
    (if (not (or date time overrides-date overrides-time)) object
        (let ((date (cond
                      ((not date) (calendar:copy (timestamp-date object) :year year :month month :day day))
                      ((and (calendar:datep date) (not overrides-date)) date)
                      ((and (calendar:timestampp date) (not overrides-date)) (timestamp-date date))
                      (t (calendar:make-date (or year (calendar:year date))
                                             (or month (calendar:month date))
                                             (or day (calendar:day date))))))
              (time (cond
                      ((not time) (calendar:copy (timestamp-time object) :hour hour :minute minute :second second :nanos nanos))
                      ((and (calendar:timep time) (not overrides-time)) time)
                      ((and (calendar:timestampp time) (not overrides-time)) (timestamp-time time))
                      (t (calendar:make-time (or hour (calendar:hour time))
                                             (or minute (calendar:minute time))
                                             (or second (calendar:second time))
                                             (or nanos (calendar:nanos time)))))))
          (make-timestamp date time)))))

(defmethod calendar:copy ((object calendar:instant) &key epoch-second nanos)
  (if (not (or epoch-second nanos)) object
      (calendar:make-instant (or epoch-second (instant-epoch-second object))
                             (or nanos (instant-nanos object)))))

(defmethod calendar:copy ((object calendar:duration) &key seconds nanos)
  (if (not (or seconds nanos)) object
      (calendar:make-duration (or seconds (duration-seconds object))
                              (or nanos (duration-nanos object)))))


(defmethod make-load-form ((object calendar:date) &optional environment)
  (declare (ignore environment))
  `(make-date ,(date-year object) ,(date-month object) ,(date-day object) ,(date-%weekday object)))

(defmethod make-load-form ((object calendar:time) &optional environment)
  (declare (ignore environment))
  `(make-time ,(time-hour object) ,(time-minute object) ,(time-second object) ,(time-nanos object)))

(defmethod make-load-form ((object calendar:timestamp) &optional environment)
  (declare (ignore environment))
  `(make-timestamp ,(timestamp-date object) ,(timestamp-time object)))

(defmethod make-load-form ((object calendar:instant) &optional environment)
  (declare (ignore environment))
  `(make-instant ,(instant-epoch-second object) ,(instant-nanos object)))

(defmethod make-load-form ((object calendar:duration) &optional environment)
  (declare (ignore environment))
  `(make-duration ,(duration-seconds object) ,(duration-nanos object)))


(macrolet ((define-equality (name &rest fields)
             `(defun ,name (object1 object2)
                (and ,@(mapcar (lambda (field)
                                 (multiple-value-bind (test getter)
                                     (if (atom field)
                                         (values 'eql field)
                                         (values (cadr field) (car field)))
                                   `(,test (,getter object1) (,getter object2))))
                               fields)))))
  (define-equality date= date-year date-month date-day)
  (define-equality time= time-hour time-minute time-second time-nanos)
  (define-equality instant= instant-epoch-second instant-nanos)
  (define-equality duration= duration-seconds duration-nanos)
  (define-equality timestamp= (timestamp-date date=) (timestamp-time time=)))

(macrolet ((define-earlierp (name &rest fields)
             (labels
                 ((walk (field fields)
                    (multiple-value-bind (getter lessp equalp)
                        (if (atom field)
                            (values field '< 'eql)
                            (values (car field) (cadr field) (caddr field)))
                      (if (null fields)
                          `(,lessp (,getter object1) (,getter object2))
                          (let ((temp1 (gensym))
                                (temp2 (gensym)))
                            `(let ((,temp1 (,getter object1))
                                   (,temp2 (,getter object2)))
                               (or (,lessp ,temp1 ,temp2)
                                   (and (,equalp ,temp1 ,temp2)
                                        ,(walk (car fields) (cdr fields))))))))))
               `(defun ,name (object1 object2)
                  ,(walk (car fields) (cdr fields))))))
  (define-earlierp date< date-year date-month date-day)
  (define-earlierp time< time-hour time-minute time-second time-nanos)
  (define-earlierp timestamp< (timestamp-date date< date=) (timestamp-time time< time=))
  (define-earlierp instant< instant-epoch-second instant-nanos)
  (define-earlierp duration< duration-seconds duration-nanos))

(declaim (inline date/= date<= date>= date> time/= time<= time>= time> timestamp/=
                 timestamp<= timestamp>= timestamp> instant/= instant<= instant>=
                 instant> duration/= duration<= duration>= duration>))

(defun date/= (object1 object2) (not (date= object1 object2)))
(defun date<= (object1 object2) (not (date< object2 object1)))
(defun date>= (object1 object2) (not (date< object1 object2)))
(defun date> (object1 object2) (date< object2 object1))

(defun time/= (object1 object2) (not (time= object1 object2)))
(defun time<= (object1 object2) (not (time< object2 object1)))
(defun time>= (object1 object2) (not (time< object1 object2)))
(defun time> (object1 object2) (time< object2 object1))

(defun timestamp/= (object1 object2) (not (timestamp= object1 object2)))
(defun timestamp<= (object1 object2) (not (timestamp< object2 object1)))
(defun timestamp>= (object1 object2) (not (timestamp< object1 object2)))
(defun timestamp> (object1 object2) (timestamp< object2 object1))

(defun instant/= (object1 object2) (not (instant= object1 object2)))
(defun instant<= (object1 object2) (not (instant< object2 object1)))
(defun instant>= (object1 object2) (not (instant< object1 object2)))
(defun instant> (object1 object2) (instant< object2 object1))

(defun duration/= (object1 object2) (not (duration= object1 object2)))
(defun duration<= (object1 object2) (not (duration< object2 object1)))
(defun duration>= (object1 object2) (not (duration< object1 object2)))
(defun duration> (object1 object2) (duration< object2 object1))

(defun date-hash (object)
  (logand most-positive-fixnum
          (+ (* 31 31 (date-year object))
             (* 31 (date-month object))
             (date-day object))))

(defun time-hash (object)
  (logand most-positive-fixnum
          (+ (* 31 31 31 (time-hour object))
             (* 31 31 (time-minute object))
             (* 31 (time-second object))
             (time-nanos object))))

(defun timestamp-hash (object)
  (logxor (logand most-positive-fixnum
                  (+ (* 31 31 (timestamp-year object))
                     (* 31 (timestamp-month object))
                     (timestamp-day object)))
          (logand most-positive-fixnum
                  (+ (* 31 31 31 (timestamp-hour object))
                     (* 31 31 (timestamp-minute object))
                     (* 31 (timestamp-second object))
                     (timestamp-nanos object)))))

(defun instant-hash (object)
  (logand most-positive-fixnum
          (+ (* 31 (instant-epoch-second object))
             (instant-nanos object))))

(defun duration-hash (object)
  (logand most-positive-fixnum
          (+ (* 31 (duration-seconds object))
             (duration-nanos object))))



(define-constant calendar:midnight (make-time 0 0 0 0) :test #'time=)
(define-constant calendar:min-time calendar:midnight :test #'time=)
(define-constant calendar:max-time (make-time 23 59 59 999999999) :test #'time=)
(define-constant calendar:noon (make-time 12 0 0 0) :test #'time=)
(define-constant calendar:min-date (make-date calendar:min-year 1 1) :test #'date=)
(define-constant calendar:max-date (make-date calendar:max-year 12 31) :test #'date=)
(define-constant calendar:epoch-date (make-date 2000 3 1) :test #'date=)
(define-constant calendar:min-timestamp (make-timestamp calendar:min-date calendar:min-time) :test #'timestamp=)
(define-constant calendar:max-timestamp (make-timestamp calendar:max-date calendar:max-time) :test #'timestamp=)
(define-constant calendar:epoch-timestamp (make-timestamp calendar:epoch-date calendar:midnight) :test #'timestamp=)
(define-constant calendar:min-instant (make-instant calendar:min-epoch-second 0) :test #'instant=)
(define-constant calendar:max-instant (make-instant calendar:max-epoch-second 999999999) :test #'instant=)
(define-constant calendar:epoch-instant (make-instant 0 0) :test #'instant=)
(define-constant calendar:zero-duration (make-duration 0 0) :test #'duration=)


(defun convert-utc-epoch-second-to-local (utc-epoch-second zone-or-offset)
  (multiple-value-bind (zone offset)
      (if (integerp zone-or-offset)
          (values nil zone-or-offset)
          (let ((zone (calendar:zone (or zone-or-offset calendar:*zone*))))
            (values zone (compute-offset utc-epoch-second nil zone))))
    (values (check-epoch-second (+ utc-epoch-second offset))
            zone offset)))

(defun convert-local-epoch-second-to-utc (local-epoch-second zone-or-offset)
  (multiple-value-bind (zone offset)
      (if (integerp zone-or-offset)
          (values nil zone-or-offset)
          (let ((zone (calendar:zone (or zone-or-offset calendar:*zone*))))
            (values zone (compute-offset local-epoch-second t zone))))
    (values (check-epoch-second (- local-epoch-second offset))
            zone offset)))


(defun calendar:now ()
  (multiple-value-bind (second nanos) (current-epoch-second-and-nanos)
    (make-instant second nanos)))

(defun calendar:current-instant ()
  (multiple-value-bind (second nanos) (current-epoch-second-and-nanos)
    (make-instant second nanos)))

(defun calendar:current-timestamp (&key zone)
  (multiple-value-bind (second nanos) (current-epoch-second-and-nanos)
    (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second (convert-utc-epoch-second-to-local second zone))
      (make-timestamp (make-date year month day weekday) (make-time hour minute second nanos)))))

(defun calendar:current-date (&key zone)
  (multiple-value-bind (second) (current-epoch-second-and-nanos)
    (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second (convert-utc-epoch-second-to-local second zone))
      (declare (ignore hour minute second))
      (make-date year month day weekday))))

(defun calendar:current-time (&key zone)
  (multiple-value-bind (second nanos) (current-epoch-second-and-nanos)
    (multiple-value-bind (year month day hour minute second) (decode-epoch-second (convert-utc-epoch-second-to-local second zone))
      (declare (ignore year month day))
      (make-time hour minute second nanos))))


(defun convert-to-instant (year month day hour minute second nanos zone)
  (make-instant (convert-local-epoch-second-to-utc (encode-epoch-second year month day hour minute second) zone)
                nanos))

(defun convert-to-instant* (year month day hour minute second nanos zone)
  (let* ((year (check-year year))
         (month (check-minute month))
         (day (check-day-of-month day year month))
         (hour (check-hour hour))
         (minute (check-minute minute))
         (second (check-second second))
         (nanos (check-nanos nanos)))
  (make-instant (convert-local-epoch-second-to-utc (encode-epoch-second year month day hour minute second) zone)
                nanos)))

(defmethod calendar:instant ((object calendar:instant) &key zone date time)
  (declare (ignore zone date time))
  object)

(defmethod calendar:instant ((object calendar:timestamp) &key zone date time)
  (declare (ignore date time))
  (convert-to-instant (timestamp-year object) (timestamp-month object) (timestamp-day object)
                      (timestamp-hour object) (timestamp-minute object) (timestamp-second object)
                      (timestamp-nanos object) zone))

(defmethod calendar:instant ((object calendar:date) &key zone date time)
  (declare (ignore date))
  (let ((time (or time calendar:midnight)))
    (if (or (calendar:timep time) (calendar:timestampp time))
        (convert-to-instant (date-year object) (date-month object) (date-day object)
                            (calendar:hour time) (calendar:minute time) (calendar:second time)
                            (calendar:nanos time) zone)
        (convert-to-instant* (date-year object) (date-month object) (date-day object)
                             (calendar:hour time) (calendar:minute time) (calendar:second time)
                             (calendar:nanos time) zone))))

(defmethod calendar:instant ((object calendar:time) &key zone date time)
  (declare (ignore time))
  (let ((date (or date calendar:epoch-date)))
    (if (or (calendar:datep date) (calendar:timestampp date))
        (convert-to-instant (calendar:year date) (calendar:month date) (calendar:day date)
                            (time-hour object) (time-minute object) (time-second object)
                            (time-nanos object) zone)
        (convert-to-instant* (calendar:year date) (calendar:month date) (calendar:day date)
                             (time-hour object) (time-minute object) (time-second object)
                             (time-nanos object) zone))))

(defmethod calendar:timestamp ((object calendar:timestamp) &key zone date time)
  (declare (ignore zone date time))
  object)

(defmethod calendar:timestamp ((object calendar:date) &key zone date time)
  (declare (ignore zone date))
  (let ((time (or time calendar:midnight)))
    (when (calendar:timestampp time) (setf time (timestamp-time time)))
    (if (calendar:timep time)
        (make-timestamp object time)
        (make-timestamp object
                        (calendar:make-time (calendar:hour time) (calendar:minute time) (calendar:second time)
                                            (calendar:nanos time))))))

(defmethod calendar:timestamp ((object calendar:time) &key zone date time)
  (declare (ignore zone time))
  (let ((date (or date calendar:epoch-date)))
    (when (calendar:timestampp date) (setf date (timestamp-date date)))
    (if (calendar:datep date)
        (make-timestamp date object)
        (make-timestamp (calendar:make-date (calendar:year date) (calendar:month date) (calendar:day date))
                        object))))

(defmethod calendar:timestamp ((object calendar:instant) &key zone date time)
  (declare (ignore date time))
  (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second (convert-utc-epoch-second-to-local (instant-epoch-second object) zone))
    (make-timestamp (make-date year month day weekday)
                    (make-time hour minute second (instant-nanos object)))))

(defmethod calendar:date ((object calendar:date) &key zone date time)
  (declare (ignore zone date time))
  object)

(defmethod calendar:date ((object calendar:time) &key zone date time)
  (declare (ignore zone time))
  (let ((date (or date calendar:epoch-date)))
    (typecase date
      (calendar:date date)
      (calendar:timestamp (timestamp-date date))
      (t (calendar:make-date (calendar:year date) (calendar:month date) (calendar:day date))))))
  
(defmethod calendar:date ((object calendar:timestamp) &key zone date time)
  (declare (ignore zone date time))
  (timestamp-date object))

(defmethod calendar:date ((object calendar:instant) &key zone date time)
  (declare (ignore date time))
  (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second (convert-utc-epoch-second-to-local (instant-epoch-second object) zone))
    (declare (ignore hour minute second))
    (make-date year month day weekday)))

(defmethod calendar:time ((object calendar:time) &key zone date time)
  (declare (ignore zone date time))
  object)

(defmethod calendar:time ((object calendar:date) &key zone date time)
  (declare (ignore zone date))
  (let ((time (or time calendar:midnight)))
    (typecase time
      (calendar:time time)
      (calendar:timestamp (timestamp-time time))
      (t (calendar:make-time (calendar:hour time) (calendar:minute time) (calendar:second time)
                             (calendar:nanos time))))))

(defmethod calendar:time ((object calendar:timestamp) &key zone date time)
  (declare (ignore zone date time))
  (timestamp-time object))

(defmethod calendar:time ((object calendar:instant) &key zone date time)
  (declare (ignore date time))
  (multiple-value-bind (year month day hour minute second) (decode-epoch-second (convert-utc-epoch-second-to-local (instant-epoch-second object) zone))
    (declare (ignore year month day))
    (make-time hour minute second (instant-nanos object))))



(defun print-date-fields (year month day stream)
  (format stream "~D-~2,'0D-~2,'0D" year month day))

(defun print-time-fields (hour minute second nanos stream)
  (if (not (plusp nanos))
      (format stream "~2,'0D:~2,'0D:~2,'0D" hour minute second)
      (let ((scale 9) (value nanos))
        (multiple-value-bind (value* rest) (floor value 1000)
          (unless (plusp rest)
            (decf scale 3)
            (setf value value*)
            (multiple-value-bind (value* rest) (floor value 1000)
              (unless (plusp rest)
                (decf scale 3)
                (setf value value*)))))
        (format stream "~2,'0D:~2,'0D:~2,'0D.~V,'0D" hour minute second scale value))))

(defun print-date (object &optional (stream *standard-output*))
  (print-date-fields (date-year object) (date-month object) (date-day object) stream)
  object)

(defun print-time (object &optional (stream *standard-output*))
  (print-time-fields (time-hour object) (time-minute object) (time-second object) (time-nanos object) stream)
  object)

(defun print-timestamp (object &optional (stream *standard-output*))
  (print-date-fields (timestamp-year object) (timestamp-month object) (timestamp-day object) stream)
  (write-char #\T stream)
  (print-time-fields (timestamp-hour object) (timestamp-minute object) (timestamp-second object) (timestamp-nanos object) stream)
  object)

(defun print-instant (object &optional (stream *standard-output*))
  (multiple-value-bind (year month day hour minute second) (decode-epoch-second (instant-epoch-second object))
    (print-date-fields year month day stream)
    (write-char #\T stream)
    (print-time-fields hour minute second (instant-nanos object) stream)
    object))

(defmethod print-object ((object calendar:date) stream)
  (if (not *print-escape*)
      (print-date object stream)
      (print-unreadable-object (object stream :type t :identity t)
        (print-date object stream))))

(defmethod print-object ((object calendar:time) stream)
  (if (not *print-escape*)
      (print-time object stream)
      (print-unreadable-object (object stream :type t :identity t)
        (print-time object stream))))

(defmethod print-object ((object calendar:timestamp) stream)
  (if (not *print-escape*)
      (print-timestamp object stream)
      (print-unreadable-object (object stream :type t :identity t)
        (print-timestamp object stream))))

(defmethod print-object ((object calendar:instant) stream)
  (if (not *print-escape*)
      (progn
        (print-instant object stream)
        (write-char #\Z stream))
      (print-unreadable-object (object stream :type t :identity t)
        (print-instant object stream)
        (write-char #\Z stream))))

(defmethod print-object ((object calendar:duration) stream)
  (if (not *print-escape*)
      (format stream "~@D~@[~@D~]" (duration-seconds object) (and (plusp (duration-nanos object)) (duration-nanos object)))
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~@D~@[~@D~]"
                (duration-seconds object)
                (and (plusp (duration-nanos object)) (duration-nanos object))))))
  


(defmethod calendar:year ((object calendar:date)) (date-year object))
(defmethod calendar:year ((object calendar:timestamp)) (timestamp-year object))

(defmethod calendar:month ((object calendar:date)) (date-month object))
(defmethod calendar:month ((object calendar:timestamp)) (timestamp-month object))

(defmethod calendar:day ((object calendar:date)) (date-day object))
(defmethod calendar:day ((object calendar:timestamp)) (timestamp-day object))

(defmethod calendar:weekday ((object calendar:date)) (date-weekday object))
(defmethod calendar:weekday ((object calendar:timestamp)) (timestamp-weekday object))

(defmethod calendar:hour ((object calendar:time)) (time-hour object))
(defmethod calendar:hour ((object calendar:timestamp)) (timestamp-hour object))

(defmethod calendar:minute ((object calendar:time)) (time-minute object))
(defmethod calendar:minute ((object calendar:timestamp)) (timestamp-minute object))

(defmethod calendar:second ((object calendar:time)) (time-second object))
(defmethod calendar:second ((object calendar:timestamp)) (timestamp-second object))

(defmethod calendar:nanos ((object calendar:time)) (time-nanos object))
(defmethod calendar:nanos ((object calendar:timestamp)) (timestamp-nanos object))
(defmethod calendar:nanos ((object calendar:instant)) (instant-nanos object))
(defmethod calendar:nanos ((object calendar:duration)) (duration-nanos object))

(defmethod calendar:epoch-second ((object calendar:instant)) (instant-epoch-second object))
(defmethod calendar:seconds ((object calendar:instant)) (instant-epoch-second object))
(defmethod calendar:seconds ((object calendar:duration)) (duration-seconds object))

(defmethod calendar:equal ((object1 calendar:date) (object2 calendar:date)) (date= object1 object2))
(defmethod calendar:equal ((object1 calendar:time) (object2 calendar:time)) (time= object1 object2))
(defmethod calendar:equal ((object1 calendar:timestamp) (object2 calendar:timestamp)) (timestamp= object1 object2))
(defmethod calendar:equal ((object1 calendar:instant) (object2 calendar:instant)) (instant= object1 object2))

(defmethod calendar:hash ((object calendar:date)) (date-hash object))
(defmethod calendar:hash ((object calendar:time)) (time-hash object))
(defmethod calendar:hash ((object calendar:timestamp)) (timestamp-hash object))
(defmethod calendar:hash ((object calendar:instant)) (instant-hash object))

(defmethod calendar:lessp ((object1 calendar:date) (object2 calendar:date)) (date< object1 object2))
(defmethod calendar:lessp ((object1 calendar:time) (object2 calendar:time)) (time< object1 object2))
(defmethod calendar:lessp ((object1 calendar:timestamp) (object2 calendar:timestamp)) (timestamp< object1 object2))
(defmethod calendar:lessp ((object1 calendar:instant) (object2 calendar:instant)) (instant< object1 object2))
(defmethod calendar:lessp ((object1 calendar:duration) (object2 calendar:duration)) (duration< object1 object2))

(defmethod calendar:day-of-year ((object calendar:timestamp))
  (calendar:day-of-year (timestamp-date object)))

(defmethod calendar:day-of-year ((object calendar:date))
  (let ((t1 '#(0 31 59 90 120 151 181 212 243 273 304 334))
        (t2 '#(0 31 60 91 121 152 182 213 244 274 305 335))
        (year (date-year object))
        (month (date-month object))
        (day (date-day object)))
    (+ day (svref (if (not (calendar:leap-year-p year)) t1 t2) (1- month)))))

(defmethod calendar:iso-week-year-and-number ((object calendar:timestamp))
  (calendar:iso-week-year-and-number (timestamp-date object)))

(defmethod calendar:iso-week-year-and-number ((object calendar:date))
  (labels
      ((iso-53-week-year-p (year)
         ;; "any year ending on Thursday (D, ED) and any leap year ending on Friday (DC)"
         (let ((day (compute-weekday year 12 31)))
           (or (eql day calendar:thursday)
               (and (calendar:leap-year-p year)
                    (eql day calendar:friday)))))
       (iso-weekday (n) (if (eql n 0) 6 (1- n))))
    (let* ((year (date-year object))
           (ordinal (calendar:day-of-year object))
           (weekday (1+ (iso-weekday (date-weekday object))))
           (number (floor (+ (- ordinal weekday) 10) 7)))
      (cond
        ((zerop number)
         (let ((py (1- year)))
           (values py (if (iso-53-week-year-p py) 53 52))))
        ((eql number 53)
         (case weekday
           ((4 5 6 7) (values year 53))
           (otherwise (values (1+ year) 1))))
        (t (values year number))))))

#+SBCL
(sb-ext:define-hash-table-test calendar:equal calendar:hash)
  


(defgeneric epoch-second-and-nanos (object)
  (:method ((object calendar:instant))
    (values (instant-epoch-second object) (instant-nanos object)))
  (:method ((object calendar:timestamp))
    (values (encode-epoch-second (timestamp-year object) (timestamp-month object) (timestamp-day object)
                                 (timestamp-hour object) (timestamp-minute object) (timestamp-second object))
            (timestamp-nanos object)))
  (:method ((object calendar:date))
    (values (encode-epoch-second (date-year object) (date-month object) (date-day object) 0 0 0)
            0))
  (:method ((object calendar:time))
    (values (encode-epoch-second 2000 3 1 (time-hour object) (time-minute object) (time-second object))
            (time-nanos object))))

(defmethod calendar:add-seconds ((object calendar:instant) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (multiple-value-call #'calendar:make-instant
        (add-raw-seconds (instant-epoch-second object) (instant-nanos object)
                         seconds nanos))))

(defmethod calendar:add-seconds ((object calendar:timestamp) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (multiple-value-bind (second* nanos*) (epoch-second-and-nanos object)
        (multiple-value-bind (second** nanos**) (add-raw-seconds second* nanos* seconds nanos)
          (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second second**)
            (make-timestamp (make-date year month day weekday)
                            (make-time hour minute second nanos**)))))))

(defmethod calendar:add-seconds ((object calendar:date) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (let ((epoch-second (encode-epoch-second (date-year object) (date-month object) (date-day object) 0 0 0)))
        (multiple-value-bind (epoch-second) (add-raw-seconds epoch-second 0 seconds nanos)
          (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second epoch-second)
            (declare (ignore hour minute second))
            (make-date year month day weekday))))))

(defmethod calendar:add-seconds ((object calendar:time) (seconds integer) &optional (nanos 0))
  (if (and (zerop seconds) (zerop nanos)) object
      (let ((epoch-second (+ (* (time-hour object) 60 60) (* (time-minute object) 60) (time-second object))))
        (multiple-value-bind (epoch-second nanos) (add-raw-seconds epoch-second (time-nanos object) seconds nanos)
          (multiple-value-bind (unused rest) (floor epoch-second seconds-per-day)
            (declare (ignore unused))
            (multiple-value-bind (rest seconds) (floor rest 60)
              (multiple-value-bind (hours minutes) (floor rest 60)
                (make-time hours minutes seconds nanos))))))))

(defmethod calendar:add-seconds ((object calendar:duration) (seconds integer) &optional (nanos 0))
  (multiple-value-bind (adjustment-seconds adjustment-nanos) (truncate nanos 1000000000)
    (calendar:make-duration (+ (duration-seconds object) seconds adjustment-seconds)
                            (+ (duration-nanos object) adjustment-nanos))))

(defun calendar:add-duration (object duration)
  (calendar:add-seconds object
                        (duration-seconds duration)
                        (duration-nanos duration)))

(defun seconds-and-nanos-between-epoch-seconds (start-second start-nanos end-second end-nanos)
  (let ((seconds (- end-second start-second))
        (nanos (- end-nanos start-nanos)))
    (cond
      ((and (plusp seconds) (minusp nanos)) (values (1- seconds) (+ nanos 1000000000)))
      (t (values seconds nanos)))))
  
(defgeneric seconds-and-nanos-between (start end))

(defmethod seconds-and-nanos-between ((start calendar:instant) (end calendar:instant))
  (seconds-and-nanos-between-epoch-seconds (instant-epoch-second start) (instant-nanos start)
                                           (instant-epoch-second end) (instant-nanos end)))

(defmethod seconds-and-nanos-between ((start calendar:timestamp) (end calendar:timestamp))
  (multiple-value-call #'seconds-and-nanos-between-epoch-seconds
    (epoch-second-and-nanos start)
    (epoch-second-and-nanos end)))

(defmethod seconds-and-nanos-between ((start calendar:time) (end calendar:time))
  (multiple-value-call #'seconds-and-nanos-between-epoch-seconds
    (epoch-second-and-nanos start)
    (epoch-second-and-nanos end)))

(defmethod seconds-and-nanos-between ((start calendar:date) (end calendar:date))
  (multiple-value-call #'seconds-and-nanos-between-epoch-seconds
    (epoch-second-and-nanos start)
    (epoch-second-and-nanos end)))
