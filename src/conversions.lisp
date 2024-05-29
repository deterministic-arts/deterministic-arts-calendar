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

(defmethod calendar:to-timestamp (object zone &rest options &key &allow-other-keys)
  (apply #'calendar:to-timestamp object (calendar:zone zone) options))

(defmethod calendar:to-timestamp (object (zone (eql t)) &rest options &key &allow-other-keys)
  (apply #'calendar:to-timestamp object (calendar:zone calendar:*zone*) options))

(defmethod calendar:to-timestamp (object (zone null) &rest options &key &allow-other-keys)
  (apply #'calendar:to-timestamp object *utc-zone* options))

(defmethod calendar:to-timestamp ((object calendar:instant) (zone calendar:zone) &key)
  (let ((offset (compute-zone-offset object zone)))
    (multiple-value-bind (year month day hour minute second weekday) (decode-epoch-second (check-epoch-second (+ (instant-epoch-second object) offset)))
      (values (make-timestamp (make-date year month day weekday) (make-time hour minute second (instant-nanos object)))
              zone offset))))

(defmethod calendar:to-instant (object zone &rest options &key &allow-other-keys)
  (apply #'calendar:to-instant object (calendar:zone zone) options))

(defmethod calendar:to-instant (object (zone (eql t)) &rest options &key &allow-other-keys)
  (apply #'calendar:to-instant object (calendar:zone calendar:*zone*) options))

(defmethod calendar:to-instant (object (zone null) &rest options &key &allow-other-keys)
  (apply #'calendar:to-instant object *utc-zone* options))

(defmethod calendar:to-instant (object (zone calendar:zone) &rest options &key &allow-other-keys)
  (apply #'calendar:to-instant (calendar:timestamp object) zone options))

(defmethod calendar:to-instant ((object calendar:timestamp) (zone calendar:zone) &key)
  (let ((offset
          (flet
              ((conversion-error-p (condition)
                 (typep condition 'calendar:conversion-error))
               (read-new-value ()
                 (lambda ()
                   (format *query-io* "~&Enter expression: ")
                   (force-output *query-io*)
                   (list (eval (read *query-io*))))))
            (restart-case (compute-zone-offset object zone)
              (calendar:return-instant (value)
                :report (lambda (stream) (format stream "enter a ~S value to return from ~S" 'calendar:instant 'calendar:to-instant))
                :test conversion-error-p
                :interactive read-new-value
                (return-from calendar:to-instant (calendar:instant value)))
              (calendar:use-offset (value)
                :report "enter a zone offset finish the computation"
                :test conversion-error-p
                :interactive read-new-value
                (etypecase value ((integer #.(* -1 26 60 60) #.(* 26 60 60)) value)))))))
    (multiple-value-bind (second nanos) (epoch-second-and-nanos object)
      (make-instant (check-epoch-second (- second offset)) nanos))))


(defun calendar:use-offset (value &optional condition)
  (let ((restart (find-restart 'calendar:use-offset condition)))
    (when restart
      (invoke-restart restart value))))

(defun calendar:return-instant (value &optional condition)
  (let ((restart (find-restart 'calendar:return-instant condition)))
    (when restart
      (invoke-restart restart value))))


(defun calendar:now ()
  (multiple-value-bind (second nanos) (current-epoch-second-and-nanos)
    (make-instant second nanos)))

(defun calendar:current-instant ()
  (multiple-value-bind (second nanos) (current-epoch-second-and-nanos)
    (make-instant second nanos)))

(defun calendar:current-timestamp (&optional (zone 't))
  (nth-value 0 (calendar:to-timestamp (calendar:now) zone)))

(defun calendar:current-date (&optional (zone 't))
  (timestamp-date (calendar:to-timestamp (calendar:now) zone)))

(defun calendar:current-time (&optional (zone 't))
  (timestamp-time (calendar:to-timestamp (calendar:now) zone)))



(defmethod calendar:instant ((object calendar:instant))
  object)

(defmethod calendar:instant ((object calendar:timestamp))
  (nth-value 0 (calendar:to-instant object t)))

(defmethod calendar:instant ((object calendar:date))
  (nth-value 0 (calendar:to-instant (calendar:timestamp object) t)))

(defmethod calendar:timestamp ((object calendar:instant))
  (nth-value 0 (calendar:to-timestamp object t)))

(defmethod calendar:timestamp ((object calendar:timestamp))
  object)

(defmethod calendar:timestamp ((object calendar:date))
  (make-timestamp object calendar:midnight))

(defmethod calendar:date ((object calendar:instant))
  (timestamp-date (calendar:to-timestamp object t)))

(defmethod calendar:date ((object calendar:timestamp))
  (timestamp-date object))

(defmethod calendar:date ((object calendar:date))
  object)

(defmethod calendar:time ((object calendar:instant))
  (timestamp-time (calendar:to-timestamp object t)))

(defmethod calendar:time ((object calendar:timestamp))
  (timestamp-time object))

(defmethod calendar:time ((object calendar:time))
  object)

