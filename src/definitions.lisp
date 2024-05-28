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

(in-package #:deterministic-arts.calendar.internals)

(defconstant calendar:sunday 0)
(defconstant calendar:monday 1)
(defconstant calendar:tuesday 2)
(defconstant calendar:wednesday 3)
(defconstant calendar:thursday 4)
(defconstant calendar:friday 5)
(defconstant calendar:saturday 6)

(defconstant lazyday 7)

(defconstant calendar:january 1)
(defconstant calendar:february 2)
(defconstant calendar:march 3)
(defconstant calendar:april 4)
(defconstant calendar:may 5)
(defconstant calendar:june 6)
(defconstant calendar:july 7)
(defconstant calendar:august 8)
(defconstant calendar:september 9)
(defconstant calendar:october 10)
(defconstant calendar:november 11)
(defconstant calendar:december 12)

(defconstant calendar:am 0)
(defconstant calendar:pm 1)

(defconstant calendar:min-year -999999999)
(defconstant calendar:max-year 999999999)
(defconstant calendar:min-epoch-second -31557015119088000)
(defconstant calendar:max-epoch-second 31556888912534399)

(defvar calendar:*zone*)

(defclass calendar:zone () ())

(defgeneric calendar:copy (object &key))

(defgeneric calendar:seconds (object))
(defgeneric calendar:zone (object))
(defgeneric calendar:expression (object))
(defgeneric calendar:year (object))
(defgeneric calendar:month (object))
(defgeneric calendar:day (object))
(defgeneric calendar:weekday (object))
(defgeneric calendar:hour (object))
(defgeneric calendar:minute (object))
(defgeneric calendar:second (object))
(defgeneric calendar:nanos (object))
(defgeneric calendar:epoch-second (object))
(defgeneric calendar:iso-week-year-and-number (object))
(defgeneric calendar:day-of-year (object))

(defgeneric calendar:to-instant (object zone &key))
(defgeneric calendar:to-timestamp (object zone &key))

(defgeneric calendar:instant (object))
(defgeneric calendar:date (object))
(defgeneric calendar:time (object))
(defgeneric calendar:timestamp (object))

(defgeneric calendar:compare (object1 object2))

(defgeneric calendar:equal (object1 object2)
  (:method (object1 object2) (equal object1 object2)))

(defgeneric calendar:hash (object)
  (:method (object) (sxhash object)))

(defgeneric calendar:add-seconds (object seconds &optional nanos))

(defgeneric resolve-zone (name &optional arguments))

(defgeneric compute-zone-offset (moment zone)
  (:argument-precedence-order zone moment))

(defgeneric epoch-second-and-nanos (object))

(deftype year ()
  `(integer ,calendar:min-year ,calendar:max-year))

(deftype calendar:epoch-second ()
  `(integer ,calendar:min-epoch-second ,calendar:max-epoch-second))


(defun calendar:millisecond (object)
  (nth-value 0 (floor (calendar:nanos object) 1000000)))

(defun calendar:microsecond (object)
  (mod (floor (calendar:nanos object) 1000) 1000))

(defun calendar:nanosecond (object)
  (mod (calendar:nanos object) 1000))

(defun calendar:earlierp (object1 object2)
  (< (calendar:compare object1 object2) 0))

(defun calendar:laterp (object1 object2)
  (> (calendar:compare object1 object2) 0))

(defun calendar:not-earlierp (object1 object2)
  (>= (calendar:compare object1 object2) 0))

(defun calendar:not-laterp (object1 object2)
  (<= (calendar:compare object1 object2) 0))

(defun calendar:shorterp (object1 object2)
  (< (calendar:compare object1 object2) 0))

(defun calendar:longerp (object1 object2)
  (> (calendar:compare object1 object2) 0))

(defun calendar:not-shorterp (object1 object2)
  (>= (calendar:compare object1 object2) 0))

(defun calendar:not-longerp (object1 object2)
  (<= (calendar:compare object1 object2) 0))

(defun calendar:subtract-seconds (object seconds &optional (nanos 0))
  (calendar:add-seconds object (- seconds) (- nanos)))

(defun calendar:iso-week-year (object)
  (nth-value 0 (calendar:iso-week-year-and-number object)))

(defun calendar:iso-week-number (object)
  (nth-value 1 (calendar:iso-week-year-and-number object)))

(defun calendar:day-period (object)
  (if (< (calendar:hour object) 12) calendar:am calendar:pm))



(define-condition calendar:conversion-error (error)
  ((timestamp :initarg :timestamp :reader calendar:conversion-error-timestamp)
   (zone :initarg :zone :reader calendar:conversion-error-zone)
   (period-start :initarg :period-start :reader calendar:conversion-error-period-start)
   (period-end :initarg :period-end :reader calendar:conversion-error-period-end)
   (candidates :initarg :candidates :initform nil :reader calendar:conversion-error-candidates)))

(define-condition calendar:ambiguous-timestamp (calendar:conversion-error)
  ()
  (:report (lambda (object stream)
             (format stream "the conversion of ~A to an instant using ~S is ambiguous due to an overlapping period between ~A and ~A during a zone offset transition~@[; ~
                             potential candidates are ~{~A~^, ~}~]"
                     (calendar:conversion-error-timestamp object)
                     (calendar:conversion-error-zone object)
                     (calendar:conversion-error-period-start object)
                     (calendar:conversion-error-period-end object)
                     (calendar:conversion-error-candidates object)))))

(define-condition calendar:undefined-timestamp (calendar:conversion-error)
  ((gap-start :initarg :gap-start :reader calendar:conversion-error-gap-start)
   (gap-end :initarg :gap-end :reader calendar:conversion-error-gap-end))
  (:report (lambda (object stream)
             (format stream "the conversion of ~A to an instant using ~S is undefined due to a skipped period between ~A and ~A during a zone offset transition~@[; ~
                             potential candidates are ~{~A~^, ~}~]"
                     (calendar:conversion-error-timestamp object)
                     (calendar:conversion-error-zone object)
                     (calendar:conversion-error-period-start object)
                     (calendar:conversion-error-period-end object)
                     (calendar:conversion-error-candidates object)))))


(defun check-month (value)
  (if (typep value '(integer 1 12)) value
      (case value
        ((:january :jan) 1)
        ((:february :feb) 2)
        ((:march :mar) 3)
        ((:april :apr) 4)
        ((:may) 5)
        ((:june :jun) 6)
        ((:july :jul) 7)
        ((:august :aug) 8)
        ((:september :sep) 9)
        ((:october :oct) 10)
        ((:november :nov) 11)
        ((:december :dec) 12)
        (otherwise (error 'simple-type-error
                          :format-control "~S is not a supported value for component ~S"
                          :format-arguments (list value 'calendar:month)
                          :datum value :expected-type `(or (integer 1 12)
                                                           (member :january :february :march :april
                                                                   :may :june :july :august :september
                                                                   :october :november :december)))))))

(defun calendar:leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))


(defun calendar:days-in-month (year month)
  (aref (if (calendar:leap-year-p year)
            '#(31 29 31 30 31 30 31 31 30 31 30 31)
            '#(31 28 31 30 31 30 31 31 30 31 30 31))
        (1- (check-month month))))


(defun check-day-of-month (value year month)
  (if (eq value :last)
      (calendar:days-in-month year month)
      (or (and (integerp value) (>= value 1)
               (or (<= value 28) (<= value (calendar:days-in-month year month)))
               value)
          (error 'simple-type-error
                 :format-control "~S is not a supported value for component ~S when the year is ~D and the month is ~S"
                 :format-arguments (list value 'calendar:day year month)
                 :datum value :expected-type `(or (eql :last) (integer 1 ,(calendar:days-in-month year month)))))))

(defun check-year (value)
  (if (and (integerp value) (<= calendar:min-year value calendar:max-year))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:year)
             :datum value :expected-type `(integer ,calendar:min-year ,calendar:max-year))))

(defun check-hour (value)
  (if (and (integerp value) (<= 0 value 23))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:hour)
             :datum value :expected-type `(integer 0 23))))

(defun check-minute (value)
  (if (and (integerp value) (<= 0 value 59))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:minute)
             :datum value :expected-type `(integer 0 59))))

(defun check-second (value)
  (if (and (integerp value) (<= 0 value 59))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:second)
             :datum value :expected-type `(integer 0 59))))

(defun check-nanos (value)
  (if (and (integerp value) (<= 0 value 999999999))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:nanos)
             :datum value :expected-type `(integer 0 999999999))))

(defun check-epoch-second (value)
  (if (and (integerp value) (<= calendar:min-epoch-second value calendar:max-epoch-second))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:epoch-second)
             :datum value :expected-type `(integer ,calendar:min-epoch-second ,calendar:max-epoch-second))))

(defun check-duration-seconds (value)
  (if (and (integerp value) (<= calendar:min-epoch-second value calendar:max-epoch-second))
      value
      (error 'simple-type-error
             :format-control "~S is not a supported value for component ~S"
             :format-arguments (list value 'calendar:seconds)
             :datum value :expected-type `(integer ,calendar:min-epoch-second ,calendar:max-epoch-second))))

(defun check-weekday (value)
  (if (and (integerp value) (<= calendar:sunday value calendar:saturday))
      value
      (case value
        ((:monday :mon) calendar:monday)
        ((:tuesday :tue) calendar:tuesday)
        ((:wednesday :wed) calendar:wednesday)
        ((:thursday :thu) calendar:thursday)
        ((:friday :fri) calendar:friday)
        ((:saturday :sat) calendar:saturday)
        (otherwise (error 'simple-type-error
                          :format-control "~S is not a supported value for component ~S"
                          :format-arguments (list value 'calendar:weekday)
                          :datum value :expected-type `(or (integer ,calendar:sunday ,calendar:saturday)
                                                           (member :sunday :monday :tuesday :wednesday
                                                                   :thursday :friday :saturday)))))))


(defconstant seconds-per-minute 60)
(defconstant seconds-per-hour (* 60 60))
(defconstant seconds-per-day (* 24 60 60))
(defconstant seconds-per-week (* 7 24 60 60))
(defconstant nanos-per-second 1000000000)
(defconstant years-per-cycle 400)
(defconstant days-per-cycle 146097)
(defconstant universal-time-offset 3160857600)
(defconstant posix-time-offset 951868800)

(defun day-of-mar1st-year (month day)
  (+ (truncate (+ (* 153 (+ month (if (> month 2) -3 9))) 2) 5) day -1))

(defun compute-weekday (year month day) 
  (let ((year (- year (if (> month 2) 2000 2001))))
    (multiple-value-bind (cycle year-of-cycle) (floor year years-per-cycle)
      (declare (ignore cycle))
      (let* ((day-of-year (day-of-mar1st-year month day))
             (day-of-cycle (- (+ (* 365 year-of-cycle) (truncate year-of-cycle 4) day-of-year) (truncate year-of-cycle 100))))
        (nth-value 1 (floor (+ 3 day-of-cycle) 7))))))

(defun encode-epoch-second (year month day hour minute second)
  (multiple-value-bind (cycle year-of-cycle) (floor (- year (if (> month 2) 2000 2001)) years-per-cycle)
    (let* ((day-of-year (day-of-mar1st-year month day))
           (day-of-cycle (- (+ (* 365 year-of-cycle) (truncate year-of-cycle 4) day-of-year) (truncate year-of-cycle 100)))
           (days (+ (* cycle days-per-cycle) day-of-cycle)))
      (declare (type (integer 0 366) day-of-year))
      (+ (* seconds-per-day days)
         (+ second (* minute seconds-per-minute) (* hour seconds-per-hour))))))

(defun decode-epoch-second (value)
  (declare (type calendar:epoch-second value))
  (multiple-value-bind (day second-of-day) (floor value seconds-per-day)
    (multiple-value-bind (cycle day-of-cycle) (floor day days-per-cycle)
      (let* ((year-of-cycle (truncate (+ day-of-cycle
                                         (- (truncate day-of-cycle 1460))
                                         (truncate day-of-cycle 36524)
                                         (- (truncate day-of-cycle (1- days-per-cycle))))
                                      365))
             (year (+ year-of-cycle (* cycle years-per-cycle))) ; not yet, but see below
             (day-of-year (+ (- day-of-cycle
                                (* 365 year-of-cycle)
                                (truncate year-of-cycle 4))
                             (truncate year-of-cycle 100)))
             (mp (truncate (+ (* 5 day-of-year) 2) 153))
             (day-of-month (1+ (- day-of-year (truncate (+ (* 153 mp) 2) 5))))
             (month (+ mp (if (< mp 10) 3 -9))))
        (multiple-value-bind (hour rest) (floor second-of-day seconds-per-hour)
          (multiple-value-bind (minute second) (floor rest seconds-per-minute)
            (values (+ year (if (> month 2) 2000 2001)) month day-of-month
                    hour minute second
                    (nth-value 1 (floor (+ 3 day-of-cycle) 7)))))))))

(defun calendar:encode-epoch-second (year month day &optional (hour 0) (minute 0) (second 0))
  (let ((year (check-year year))
        (month (check-month month)))
    (encode-epoch-second year month (check-day-of-month day year month)
                         (check-hour hour) (check-minute minute) (check-second second))))

(defun calendar:decode-epoch-second (value)
  (decode-epoch-second (check-epoch-second value)))



;;;
;;; This code was shamelessly stolen from LOCAL-TIME. Maybe, we should
;;; require local-time as dependency for now, and do local-time::%get-current-time?
;;;

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Allegro common lisp requires some toplevel hoops through which to
  ;; jump in order to call unix's gettimeofday properly.
  (ff:def-foreign-type timeval
      (:struct (tv_sec :long)
               (tv_usec :long)))

  (ff:def-foreign-call
      (allegro-ffi-gettimeofday "gettimeofday")
      ((timeval (* timeval))
       ;; and do this to allow a 0 for NULL
       (timezone :foreign-address))
    :returning (:int fixnum)))

(defun %get-current-time ()
  "Cross-implementation abstraction to get the current time measured from the unix epoch (1/1/1970). Should return (values sec nano-sec)."
  #+allegro
  (flet ((allegro-gettimeofday ()
           (let ((tv (ff:allocate-fobject 'timeval :c)))
             (allegro-ffi-gettimeofday tv 0)
             (let ((sec (ff:fslot-value-typed 'timeval :c tv 'tv_sec))
                   (usec (ff:fslot-value-typed 'timeval :c tv 'tv_usec)))
               (ff:free-fobject tv)
               (values sec usec)))))
    (multiple-value-bind (sec usec) (allegro-gettimeofday)
      (values sec (* 1000 usec))))
  #+cmu
  (multiple-value-bind (success? sec usec) (unix:unix-gettimeofday)
    (assert success? () "unix:unix-gettimeofday reported failure?!")
    (values sec (* 1000 usec)))
  #+sbcl
  (progn
    (multiple-value-bind (sec nsec) (sb-ext:get-time-of-day)
      (values sec (* 1000 nsec))))
  #+(and ccl (not windows))
  (ccl:rlet ((tv :timeval))
    (let ((err (ccl:external-call "gettimeofday" :address tv :address (ccl:%null-ptr) :int)))
      (assert (zerop err) nil "gettimeofday failed")
      (values (ccl:pref tv :timeval.tv_sec) (* 1000 (ccl:pref tv :timeval.tv_usec)))))
  #+abcl
  (multiple-value-bind (sec millis)
      (truncate (java:jstatic "currentTimeMillis" "java.lang.System") 1000)
    (values sec (* millis 1000000)))
  #-(or allegro cmu sbcl abcl (and ccl (not windows)))
  (values (- (get-universal-time)
             ;; CL's get-universal-time uses an epoch of 1/1/1900, so adjust the result to the Unix epoch
             #.(encode-universal-time 0 0 0 1 1 1970 0))
          0))

(defun current-epoch-second-and-nanos ()
  (multiple-value-bind (seconds-since-unix nanos) (%get-current-time)
    (values (- seconds-since-unix posix-time-offset)
            nanos)))


(defun add-or-subtract (object sign adjustments)
  (let ((seconds 0) (nanos 0))
    (loop
      for (unit value) on adjustments by #'cddr
      do (ecase unit
           ((:duration) (incf seconds (calendar:seconds value)) (incf nanos (calendar:nanos value)))
           ((:nanoseconds :nsecs) (incf nanos value))
           ((:microsends :usecs) (incf nanos (* value 1000)))
           ((:milliseconds :msecs) (incf nanos (* value 1000000)))
           ((:seconds :secs) (incf seconds value))
           ((:minutes :mins) (incf seconds (* value seconds-per-minute)))
           ((:hours) (incf seconds (* value seconds-per-hour)))
           ((:days) (incf seconds (* value seconds-per-day)))
           ((:week) (incf seconds (* value seconds-per-day 7)))))
    (if (and (zerop seconds) (zerop nanos)) object
        (calendar:add-seconds object (* sign seconds) (* sign nanos)))))

(defun calendar:add (object &rest adjustments)
  (add-or-subtract object 1 adjustments))

(defun calendar:subtract (object &rest adjustments)
  (add-or-subtract object -1 adjustments))

(defun add-raw-seconds (seconds1 nanos1 seconds2 nanos2)
  (incf seconds1 seconds2)
  (incf nanos1 nanos2)
  (multiple-value-bind (adjustment-seconds adjustment-nanos) (truncate nanos1 1000000000)
    (incf seconds1 adjustment-seconds)
    (incf nanos1 adjustment-nanos)
    (multiple-value-bind (cut-seconds cut-nanos) (floor adjustment-nanos 1000000000)
      (values (+ seconds1 cut-seconds) cut-nanos))))
