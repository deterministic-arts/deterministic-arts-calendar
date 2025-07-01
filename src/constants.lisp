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

;; These constants are defined really late, with ASDF being instructed to
;; load/compile this file only after having loaded the rest of the system.
;; We want them to be available for clients of the library, but do not use
;; them ourselves (for that, we have the +XXX+ versions, for which we can
;; control that nobody modifies the values)
;;
;; The lisp system is allowed to evaluate the constant values at compile-time,
;; which is the reason for these shenanigans: during compile time, a form
;;
;;  (defconstant calendar:min-time (make-time ...))
;;
;; may fail, since the function make-time is not defined (yet). Further
;; more, the compiler might need the help of the make-load-form methods
;; in order to dump the values into the fasl which are also not (yet)
;; defined during compile time.
;;
;; So, internally, we use the +xxx+ variables, carefully ensuring, that
;; those are never re-assigned or bound. For the application using this
;; library, we want the values exposed as constants, though. Hence the
;; trick with the early and late modules in the ASDF file. Almost everything
;; is in "early". After early is compiled it is loaded to provide the machinery
;; required in order for the compiler to be able to handle the following
;; definitions.
;;
;; To the outside world, nothing special is going on. The constants are
;; provided like any other constants, and all the hacking is gone.

(defconstant calendar:min-time +min-time+)
(defconstant calendar:max-time +max-time+)

(defconstant calendar:min-date +min-date+)
(defconstant calendar:max-date +max-date+)

(defconstant calendar:min-timestamp +min-timestamp+)
(defconstant calendar:max-timestamp +max-timestamp+)

(defconstant calendar:min-instant +min-instant+)
(defconstant calendar:max-instant +max-instant+)

(defconstant calendar:midnight +midnight+)
(defconstant calendar:noon +noon+)

(defconstant calendar:epoch-date +epoch-date+)
(defconstant calendar:epoch-timestamp +epoch-timestamp+)
(defconstant calendar:epoch-instant +epoch-instant+)

(defconstant calendar:zero-duration +zero-duration+)
(defconstant calendar:epsilon-duration +epsilon-duration+)

(defconstant calendar:utc-zone +utc-zone+)
