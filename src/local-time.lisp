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

(defpackage #:deterministic-arts.calendar.local-time-support
  (:use #:common-lisp)
  (:local-nicknames (#:lt #:local-time) (#:c #:deterministic-arts.calendar) (#:ci #:deterministic-arts.calendar.internals))
  (:export #:to-local-time))

(in-package #:deterministic-arts.calendar.local-time-support)

(defmethod c:epoch-second ((object lt:timestamp) &optional zone)
  (declare (ignore zone))
  (+ (* (lt:day-of object) ci::seconds-per-day) (lt:sec-of object)))

(defmethod c:instant ((object lt:timestamp))
  (c:make-instant (+ (* (lt:day-of object) ci::seconds-per-day) (lt:sec-of object))
                  (lt:nsec-of object)))

(defmethod c:timestamp ((object lt:timestamp))
  (c:timestamp (c:instant object)))

(defmethod c:date ((object lt:timestamp))
  (c:date (c:instant object)))

(defmethod c:time ((object lt:timestamp))
  (c:time (c:instant object)))

(defgeneric to-local-time (object &key zone date time))

(defmethod to-local-time ((object c:instant) &key zone date time)
  (declare (ignore zone date time))
  (multiple-value-bind (day sec) (floor (ci::instant-epoch-second object) ci::seconds-per-day)
    (make-instance 'lt:timestamp :day day :sec sec
                                 :nsec (ci::instant-nanos object))))

(defmethod to-local-time ((object c:timestamp) &rest options &key zone date time)
  (declare (ignore zone date time))
  (to-local-time (apply #'c:instant object options)))

(defmethod to-local-time ((object c:date) &rest options &key zone date time)
  (declare (ignore zone date time))
  (to-local-time (apply #'c:instant object options)))

(defmethod to-local-time ((object c:time) &rest options &key zone date time)
  (declare (ignore zone date time))
  (to-local-time (apply #'c:instant object options)))

  
