;;;; -*- Mode: lisp -*-
;;;;
;;;; Copyright (c) 2007, 2008, 2011 Raymond Toy
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package #:oct)

(defparameter +pi+
  (make-instance 'qd-real :value octi:+qd-pi+)
  "Pi represented as a QD-REAL")

(defparameter +pi/2+
  (make-instance 'qd-real :value octi:+qd-pi/2+)
  "Pi/2 represented as a QD-REAL")

(defparameter +pi/4+
  (make-instance 'qd-real :value octi:+qd-pi/4+)
  "Pi/4 represented as a QD-REAL")

(defparameter +2pi+
  (make-instance 'qd-real :value octi:+qd-2pi+)
  "2*pi represented as a QD-REAL")

(defparameter +log2+
  (make-instance 'qd-real :value octi:+qd-log2+)
  "Natural log of 2 represented as a QD-REAL")

;; How do we represent infinity for a QD-REAL?  For now, we just make
;; the QD-REAL whose most significant part is infinity.  Currently
;; only supported on CMUCL.
#+cmu
(defparameter +quad-double-float-positive-infinity+
  (make-instance 'qd-real :value (make-qd-d ext:double-float-positive-infinity))
  "One representation of positive infinity for QD-REAL")

#+cmu
(defparameter +quad-double-float-negative-infinity+
  (make-instance 'qd-real :value (make-qd-d ext:double-float-negative-infinity))
  "One representation of negative infinity for QD-REAL")

(defparameter +most-positive-quad-double-float+
  (make-instance 'qd-real
		 :value (octi::%make-qd-d most-positive-double-float
					 (cl:scale-float most-positive-double-float (cl:* 1 -53))
					 (cl:scale-float most-positive-double-float (cl:* 2 -53))
					 (cl:scale-float most-positive-double-float (cl:* 3 -53))))
  "Most positive representable QD-REAL")

(defparameter +least-positive-quad-double-float+
  (make-instance 'qd-real
		 :value (make-qd-d least-positive-double-float))
  "Least positive QD-REAL")

;; Not sure this is 100% correct, but I think if the first component
;; is any smaller than this, the last component would no longer be a
;; normalized double-float.
(defparameter +least-positive-normalized-quad-double-float+
  (make-instance 'qd-real
		 :value (make-qd-d (cl:scale-float least-positive-normalized-double-float (cl:* 3 53))))
  "Least positive normalized QD-REAL")

(defparameter +qd-real-one+
  (make-instance 'qd-real :value (make-qd-d 1d0))
  "QD-REAL representation of 1")

(defparameter +%gamma+
  (make-instance 'qd-real :value octi::+qd-%gamma+)
  "Euler's constant")