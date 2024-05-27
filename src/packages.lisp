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

(defpackage #:deterministic-arts.calendar
  (:use)
  (:export #:date #:datep #:make-date #:time #:timep #:make-time #:timestamp
           #:timestampp #:make-timestamp #:instant #:instantp #:make-instant
           #:zone #:zonep #:year #:month #:day #:weekday #:hour #:minute #:second
           #:seconds #:millisecond #:microsecond #:nanosecond #:nanos #:expression
           #:epoch-second #:encode-epoch-second #:decode-epoch-second #:equal
           #:hash #:compare #:monday #:tuesday #:wednesday #:thursday #:friday
           #:saturday #:sunday #:january #:february #:march #:april #:may #:june
           #:july #:august #:september #:october #:november #:december #:leap-year-p
           #:days-in-month #:min-year #:max-year #:min-epoch-second #:max-epoch-second
           #:now #:*zone* #:current-instant #:current-date #:current-time #:current-timestamp
           #:add-seconds #:add-duration #:subtract-seconds #:add #:subtract #:duration
           #:durationp #:make-duration #:duration-plusp #:duration-minusp #:duration-zerop
           #:min-time #:max-time #:midnight #:noon #:min-date #:max-date #:epoch-date
           #:min-timestamp #:max-timestamp #:epoch-timestamp #:min-instant
           #:max-instant #:epoch-instant #:zero-duration #:iso-week-year
           #:iso-week-number #:iso-week-year-and-number #:day-of-year #:copy
           #:earlierp #:laterp #:not-earlierp #:not-laterp #:shorterp #:longerp
           #:not-shorterp #:not-longerp #:am #:pm #:day-period))

(defpackage #:deterministic-arts.calendar.internals
  (:use #:common-lisp #:alexandria)
  (:local-nicknames (#:calendar #:deterministic-arts.calendar))
  (:export #:resolve-zone #:compute-zone-offset))

