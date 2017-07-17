;;;; -*- Mode: lisp -*-
;;;;
;;;; Copyright (c) 2007 Raymond Toy
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


;;; Some simple timing tests
(in-package #:oct)

(defun time-add (&optional (n 100000))
  (declare (fixnum n))
  (flet ((sum-double ()
	   (let ((sum 0d0))
	     (declare (double-float sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (cl:+ sum 1d0)))
	     sum))
	 (sum-%qd ()
	   (let ((sum (octi::make-qd-d 0d0))
		 (one (octi::make-qd-d 1d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (add-qd sum one)))
	     sum))
	 (sum-qd ()
	   (let ((sum #q0))
	     (declare (type qd-real sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (+ sum #q1)))
	     sum)))
    (format t "Add double-floats ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (sum-double))
    (format t "Add %quad-double (internal) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (sum-%qd))
    (format t "Add QD-REAL (method) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (sum-qd))))


(defun time-mul (&optional (n 100000))
  (declare (fixnum n))
  (flet ((mul-double ()
	   (let ((sum 0d0))
	     (declare (double-float sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (cl:* sum 1d0)))
	     sum))
	 (mul-%qd ()
	   (let ((sum (octi::make-qd-d 0d0))
		 (one (octi::make-qd-d 1d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (mul-qd sum one)))
	     sum))
	 (mul-qd ()
	   (let ((sum #q0))
	     (declare (type qd-real sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (* sum #q1)))
	     sum)))
    (format t "Multiply double-floats ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (mul-double))
    (format t "Multiply %quad-double (internal) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (mul-%qd))
    (format t "Multiply QD-REAL (method) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (mul-qd))))

(defun time-div (&optional (n 100000))
  (declare (fixnum n))
  (flet ((div-double ()
	   (let ((sum 7d0))
	     (declare (double-float sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (cl:/ sum 1d0)))
	     sum))
	 (div-%qd ()
	   (let ((sum (octi::make-qd-d 7d0))
		 (one (octi::make-qd-d 1d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (div-qd sum one)))
	     sum))
	 (div-qd ()
	   (let ((sum #q7))
	     (declare (type qd-real sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (/ sum #q1)))
	     sum)))
    (format t "Divide double-floats ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (div-double))
    (format t "Divide %quad-double (internal) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (div-%qd))
    (format t "Divide QD-REAL (method) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (div-qd))))

(defun time-sqrt (&optional (n 100000))
  (declare (fixnum n))
  (flet ((sqrt-double ()
	   (let ((sum 7d0))
	     (declare (double-float sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (cl:sqrt sum)))
	     sum))
	 (sqrt-%qd ()
	   (let ((sum (octi::make-qd-d 7d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (sqrt-qd sum)))
	     sum))
	 (sqrt-qd-real ()
	   (let ((sum #q7))
	     (declare (type qd-real sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (sqrt sum)))
	     sum)))
    (format t "Sqrt double-floats ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (sqrt-double))
    (format t "Sqrt %quad-double (internal) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (sqrt-%qd))
    (format t "Sqrt QD-REAL (method) ~d times~%" n)
    #+cmu (ext:gc :full t)
    (time (sqrt-qd-real))))

(defun time-atan (&optional (n 10000))
  (declare (fixnum n))
  (flet ((time-atan/newton ()
	   (let ((sum (octi::make-qd-d 0.01d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (octi::atan-qd/newton sum)))
	     sum))
	 (time-atan/taylor ()
	   (let ((sum (octi::make-qd-d 0.01d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (octi::atan-qd/taylor sum)))
	     sum)))
    (format t "atan-qd/newton ~d times~%" n)
    #+cmu (ext:gc :full t)
    (format t "sum = ~A~%" (time (time-atan/newton)))
    (format t "atan-qd/taylor ~d times~%" n)
    #+cmu (ext:gc :full t)
    (format t "sum = ~A~%" (time (time-atan/taylor)))))

;; Some timing results on an iMac, 3.06 GHz Core i3:

; atan-qd/newton 10000 times

; Evaluation took:
;   6.15 seconds of real time
;   6.116626 seconds of user run time
;   0.021624 seconds of system run time
;   18,805,071,897 CPU cycles
;   [Run times include 0.24 seconds GC run time]
;   0 page faults and
;   893,037,112 bytes consed.
; 
; sum = #C(0.0077459785628163552318041722744135w0 5.8166227464838760117515984152653w-36)
;
; atan-qd/taylor 10000 times

; Evaluation took:
;   0.29 seconds of real time
;   0.28987 seconds of user run time
;   0.001629 seconds of system run time
;   892,698,785 CPU cycles
;   [Run times include 0.02 seconds GC run time]
;   0 page faults and
;   79,108,384 bytes consed.
; 
; sum = #C(0.0077459785628163552318041722744135w0 5.81662274648387601175165266362535w-36)

;; We see that the taylor series is 21 times faster (!) and conses 11
;; times less.  That's a pretty nice gain, at the expense of two
;; 1024-element tables.

(defun time-atan2 (&optional (n 10000))
  (declare (fixnum n))
  (flet ((time-atan/newton ()
	   (let ((sum (octi::make-qd-d -1000d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (octi::atan2-qd/newton sum (mul-qd-d sum .1d0))))
	     sum))
	 (time-atan/taylor ()
	   (let ((sum (octi::make-qd-d -1000d0)))
	     (declare (type octi::%quad-double sum)
		      (optimize (speed 3)))
	     (dotimes (k n)
	       (declare (fixnum k))
	       (setf sum (octi::atan2-qd/taylor sum (mul-qd-d sum .1d0))))
	     sum)))
    (format t "atan2-qd/newton ~d times~%" n)
    #+cmu (ext:gc :full t)
    (format t "sum = ~A~%" (time (time-atan/newton)))
    (format t "atan2-qd/taylor ~d times~%" n)
    #+cmu (ext:gc :full t)
    (format t "sum = ~A~%" (time (time-atan/taylor)))))


;; Some timing results on an iMac, 3.06 GHz Core i3:

; atan2-qd/newton 10000 times

; Evaluation took:
;   6.37 seconds of real time
;   6.321544 seconds of user run time
;   0.026819 seconds of system run time
;   19,474,336,804 CPU cycles
;   [Run times include 0.27 seconds GC run time]
;   0 page faults and
;   934,872,144 bytes consed.
; 
; sum = #C(-1.670464979286058652105921398771024w0 1.39712341620073544419882362664172w-33)
;
; atan2-qd/taylor 10000 times

; Evaluation took:
;   0.32 seconds of real time
;   0.320343 seconds of user run time
;   0.001606 seconds of system run time
;   989,558,772 CPU cycles
;   [Run times include 0.02 seconds GC run time]
;   0 page faults and
;   88,919,384 bytes consed.
; 
; sum = #C(-1.670464979286058652105921398771027w0 4.47861132722031280908838833477765w-33)
