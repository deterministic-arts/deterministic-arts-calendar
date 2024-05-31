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
           #:durationp #:make-duration #:epsilon-duration #:min-time #:max-time
           #:midnight #:noon #:min-date #:max-date #:epoch-date #:min-timestamp
           #:max-timestamp #:epoch-timestamp #:min-instant #:max-instant #:epoch-instant
           #:zero-duration #:iso-week-year #:iso-week-number #:iso-week-year-and-number
           #:day-of-year #:copy #:earlierp #:laterp #:not-earlierp #:not-laterp
           #:shorterp #:longerp #:not-shorterp #:not-longerp #:am #:pm #:day-period
           #:to-instant #:to-timestamp #:ambiguous-timestamp #:undefined-timestamp
           #:conversion-error #:conversion-error-timestamp #:conversion-error-zone
           #:conversion-error-candidates #:conversion-error-period-start
           #:conversion-error-period-end #:return-instant #:use-offset #:plusp
           #:minusp #:zerop #:offset-before #:offset-after #:transition #:transitionp)
  (:documentation "The consumer-level API of this library."))

(defpackage #:deterministic-arts.calendar.internals
  (:use #:common-lisp #:alexandria #:bordeaux-threads)
  (:import-from #:trivial-garbage #:make-weak-hash-table)
  (:local-nicknames (#:calendar #:deterministic-arts.calendar))
  (:export #:date-year #:date-month #:date-day #:date-weekday #:time-hour
           #:time-minute #:time-second #:time-nanos #:instant-epoch-second
           #:instant-nanos #:duration-seconds #:duration-nanos #:timestamp-year
           #:timestamp-month #:timestamp-day #:timestamp-hour #:timestamp-minute
           #:timestamp-second #:timestamp-nanos #:timestamp-weekday #:compare-dates
           #:compare-times #:compare-timestamps #:compare-instants #:compare-durations
           #:date-hash #:time-hash #:timestamp-hash #:instant-hash #:duration-hash
           #:date= #:date/= #:date< #:date<= #:date>= #:date> #:time= #:time/=
           #:time< #:time<= #:time>= #:time> #:timestamp= #:timestamp/=
           #:timestamp< #:timestamp<= #:timestamp>= #:timestamp> #:duration=
           #:duration/= #:duration< #:duration<= #:duration>= #:duration>
           #:duration-minusp #:duration-zerop #:duration-plusp
           #:resolve-zone #:compute-zone-offset #:utc-epoch-second-and-nanos
           #:transition-timestamp #:transition-epoch-second #:transition-offset-before
           #:transition-offset-after #:transition-duration #:transition-gap-p
           #:transition-overlap-p)
  (:documentation "The service-provider-level API of this library. May also be
occasionally useful for plain consumers, but mostly intended to be used by code
implementing new time zones, etc."))

