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



(defun read-qd-real-or-complex (stream)
  (let ((c (peek-char t stream)))
    (cond ((char= c #\()
	   ;; Read a QD complex
	   (read-char stream)		; Skip the paren
	   (let ((real (read stream t nil t))
		 (imag (read stream t nil t)))
	     (unless (char= (peek-char t stream) #\))
	       (error "Illegal QD-COMPLEX number format"))
	     ;; Read closing paren
	     (read-char stream)
	     (make-instance 'qd-complex
			    :real (qd-value (float real +qd-real-one+))
			    :imag (qd-value (float imag +qd-real-one+)))))
	  (t
	   (make-instance 'qd-real :value (read-qd stream))))))
	
(defun qd-class-reader (stream subchar arg)
  (declare (ignore subchar))
  (when arg
    (warn "Numeric argument ignored in #~DQ" arg))
  (read-qd-real-or-complex stream))

(defvar *oct-readtable*
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\Q #'qd-class-reader rt)
    rt)
  "Readtable that extends the standard readtable to include #q for
  reading QD-REAL and QD-COMPLEX numbers")
