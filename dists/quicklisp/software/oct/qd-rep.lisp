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

(in-package #:octi)

;;; This file contains the actual representation of a %quad-double
;;; number.  The only real requirement for a %quad-double number is an
;;; object that can hold four double-float values.
;;;
;;; This object is created by %MAKE-QD-D.  The four double-float
;;; elements of a %quad-double are accessed via QD-0, QD-1, QD-2, and
;;; QD-3.  A convenience function, QD-PARTS, is also provided to
;;; return all four values at once.

;; All of the following functions should be inline to reduce consing.
#+(and cmu (not oct-array))
(declaim (inline
	  qd-0 qd-1 qd-2 qd-3
	  %make-qd-d
	  qd-parts))

#+(and cmu (not oct-array))
(progn
;; For CMUCL (at least recent enough versions that support
;; double-double-float), we can use a (complex double-double-float) to
;; hold our 4 double-float values.  This has a nice advantage: Much of
;; the arithmetic can be done without consing, provided the key
;; functions are inline.
(deftype %quad-double ()
  '(complex double-double-float))

;; QD-0, QD-1, QD-2, and QD-3 extract the various parts of a
;; quad-double.  QD-0 is the most significant part and QD-3 is the
;; least.
(defun qd-0 (q)
  "Return the most significant double-float in the %QUAD-DOUBLE number Q"
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (kernel:double-double-hi (realpart q)))
(defun qd-1 (q)
  "Return the second most significant double-float in the %QUAD-DOUBLE number Q"
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (kernel:double-double-lo (realpart q)))
(defun qd-2 (q)
  "Return the third most significant double-float in the %QUAD-DOUBLE number Q"
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (kernel:double-double-hi (imagpart q)))
(defun qd-3 (q)
  "Return the fourth most significant (least significant) double-float in the
 %QUAD-DOUBLE number Q"
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (kernel:double-double-lo (imagpart q)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %make-qd-d (a0 a1 a2 a3)
  "Make a %QUAD-DOUBLE from four double-floats, exactly using the given
  values.  No check is made to see if the values make sense.  A0 is
  the most significant part and A3 is the least.
" 
  (declare (double-float a0 a1
			 a2 a3))
  (complex (kernel:%make-double-double-float a0 a1)
	   (kernel:%make-double-double-float a2 a3)))
)

(defmacro %store-qd-d (target q0 q1 q2 q3)
  (declare (ignore target))
  `(%make-qd-d ,q0 ,q1, q2, q3))

(defun qd-parts (qd)
  "Extract the four doubles comprising a quad-double and return them
  as multiple values.  The most significant double is the first value."
  (declare (type %quad-double qd))
  (let ((re (realpart qd))
	(im (imagpart qd)))
    (values (kernel:double-double-hi re)
	    (kernel:double-double-lo re)
	    (kernel:double-double-hi im)
	    (kernel:double-double-lo im))))

) ; end progn

#+oct-array
(progn
;; For Lisp's without a double-double-float type, I think the best we
;; can do is a simple-array of four double-floats.  Even with
;; inlining, I think there will lots of consing when working with this
;; type.
;; 
;; A defstruct would also work but I think a simple-array is the
;; simplest and smallest representation.
(deftype %quad-double ()
  '(simple-array double-float (4)))

#||
(defun qd-0 (q)
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (aref q 0))

(defun qd-1 (q)
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (aref q 1))

(defun qd-2 (q)
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (aref q 2))

(defun qd-3 (q)
  (declare (type %quad-double q)
	   (optimize (speed 3)))
  (aref q 3))

||#

(defmacro qd-0 (q)
  `(aref ,q 0))

(defmacro qd-1 (q)
  `(aref ,q 1))

(defmacro qd-2 (q)
  `(aref ,q 2))

(defmacro qd-3 (q)
  `(aref ,q 3))

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %make-qd-d (a0 a1 a2 a3)
  "Make a %quad-double from 4 double-floats, exactly using the given
  values.  No check is made to see if the values make sense.  A0 is
  the most significant part and A3 is the least.
" 
  (declare (double-float a0 a1
			 a2 a3)
	   (optimize (speed 3)))
  (let ((a (make-array 4 :element-type 'double-float)))
    (setf (aref a 0) a0)
    (setf (aref a 1) a1)
    (setf (aref a 2) a2)
    (setf (aref a 3) a3)
    a))
)

(defmacro %make-qd-d (a0 a1 a2 a3)
  (let ((a (gensym)))
    `(let ((,a (make-array 4 :element-type 'double-float)))
      (setf (aref ,a 0) ,a0)
      (setf (aref ,a 1) ,a1)
      (setf (aref ,a 2) ,a2)
      (setf (aref ,a 3) ,a3)
      ,a)))

(defmacro %store-qd-d (target q0 q1 q2 q3)
  (let ((dest (gensym "TARGET-")))
    `(let ((,dest ,target))
       (declare (type %quad-double ,dest))
       (setf (aref ,dest 0) ,q0)
       (setf (aref ,dest 1) ,q1)
       (setf (aref ,dest 2) ,q2)
       (setf (aref ,dest 3) ,q3)
       ,dest)))

(defun qd-parts (qd)
  "Extract the four doubles comprising a quad-double and return them
  as multiple values.  The most significant double is the first value."
  (declare (type %quad-double qd))
  (values (aref qd 0)
	  (aref qd 1)
	  (aref qd 2)
	  (aref qd 3)))

) ; end progn

(defmacro with-qd-parts ((a0 a1 a2 a3) qd &body body)
  (let ((q (gensym)))
    `(let* ((,q ,qd)
	    (,a0 (qd-0 ,q))
	    (,a1 (qd-1 ,q))
	    (,a2 (qd-2 ,q))
	    (,a3 (qd-3 ,q)))
      ,@body)))


;; Some simple support for infinity and NaN.  For CMUCL, we can import
;; the desired functions from the EXTENSIONS package.

;; Implementation for Allegro
#+allegro
(progn
(defmacro float-infinity-p (x)
  `(= (abs ,x) #.excl::*infinity-double*))

(defun float-nan-p (x)
  (excl::nan-p x))

(defun float-trapping-nan-p (x)
  nil)
) ; end progn


;; Default implementation.  Assume we can't recognize any of these.

#-(or cmu allegro)
(progn
(defun float-infinity-p (x)
  (declare (ignore x))
  nil)
(defun float-nan-p (x)
  (declare (ignore x))
  nil)
(defun float-trapping-nan-p (x)
  (declare (ignore x))
  nil)
) ; end progn


;; Define some compiler macros to transform add-qd to add-qd-t
;; directly.  For CMU without :oct-array, we always replace the
;; parameter C with NIL because we don't use it.  For other Lisps, we
;; create the necessary object and call add-qd-t.
;;
;; Do the same mul-qd and other similar functions.
(macrolet
    ((frob (qd qd-t)
       #-oct-array
       `(define-compiler-macro ,qd (a b &optional c)
	  (if c
	      `(setf ,c (,',qd-t ,a ,b nil))
	      `(,',qd-t ,a ,b nil)))
       #+oct-array
       `(define-compiler-macro ,qd (a b &optional c)
	  (if c
	      `(,',qd-t ,a ,b ,c)
	      `(,',qd-t ,a ,b (%make-qd-d 0d0 0d0 0d0 0d0))))))
  (frob add-qd add-qd-t)
  (frob mul-qd mul-qd-t)
  (frob div-qd div-qd-t)
  (frob add-qd-d add-qd-d-t)
  (frob mul-qd-d mul-qd-d-t))

#+(and cmu (not oct-array))
(define-compiler-macro sub-qd (a b &optional c)
  (if c
      `(setf ,c (add-qd-t ,a (neg-qd ,b) nil))
      `(add-qd-t ,a (neg-qd ,b) nil)))

#-(and cmu (not oct-array))
(define-compiler-macro sub-qd (a b &optional c)
  (if c
      `(add-qd-t ,a (neg-qd ,b) ,c)
      `(add-qd-t ,a (neg-qd ,b) (%make-qd-d 0d0 0d0 0d0 0d0))))

#+(and cmu (not oct-array))
(define-compiler-macro sqr-qd (a &optional c)
  (if c
      `(setf ,c (sqr-qd-t ,a nil))
      `(sqr-qd-t ,a nil)))

#-(and cmu (not oct-array))
(define-compiler-macro sqr-qd (a &optional c)
  (if c
      `(sqr-qd-t ,a ,c)
      `(sqr-qd-t ,a (%make-qd-d 0d0 0d0 0d0 0d0))))

#+(and cmu (not oct-array))
(define-compiler-macro add-d-qd (a b &optional c)
  (if c
      `(setf ,c (add-qd-d ,b ,a))
      `(add-qd-d ,b ,a)))

#-(and cmu (not oct-array))
(define-compiler-macro add-d-qd (a b &optional c)
  (if c
      `(add-qd-d ,b ,a ,c)
      `(add-qd-d ,b ,a (%make-qd-d 0d0 0d0 0d0 0d0))))

#+(and cmu (not oct-array))
(define-compiler-macro sub-qd-d (a b &optional c)
  (if c
      `(setf ,c (add-qd-d ,a (cl:- ,b)))
      `(add-qd-d ,a (cl:- ,b))))

#-(and cmu (not oct-array))
(define-compiler-macro sub-qd-d (a b &optional c)
  (if c 
      `(add-qd-d ,a (cl:- ,b) ,c)
      `(add-qd-d ,a (cl:- ,b) (%make-qd-d 0d0 0d0 0d0 0d0))))

#+(and cmu (not oct-array))
(define-compiler-macro sub-d-qd (a b &optional c)
  (if c
      `(setf ,c (add-d-qd ,a (neg-qd ,b)))
      `(add-d-qd ,a (neg-qd ,b))))

#-(and cmu (not oct-array))
(define-compiler-macro sub-d-qd (a b &optional c)
  (if c
      `(add-d-qd ,a (neg-qd ,b) ,c)
      `(add-d-qd ,a (neg-qd ,b) (%make-qd-d 0d0 0d0 0d0 0d0))))

#+(and cmu (not oct-array))
(define-compiler-macro neg-qd (a &optional c)
  (if c
      `(setf ,c (neg-qd-t ,a nil))
      `(neg-qd-t ,a nil)))

#-(and cmu (not oct-array))
(define-compiler-macro neg-qd (a &optional c)
  (if c
      `(neg-qd-t ,a ,c)
      `(neg-qd-t ,a (%make-qd-d 0d0 0d0 0d0 0d0))))

