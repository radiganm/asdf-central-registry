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

(defmethod make-qd ((x cl:rational))
  ;; We should do something better than this.
  (make-instance 'qd-real :value (rational-to-qd x)))

;; Determine which of x and y has the higher precision and return the
;; value of the higher precision number.  If both x and y are
;; rationals, just return 1f0, for a single-float value.
(defun float-contagion-2 (x y)
  (etypecase x
    (cl:rational
     (etypecase y
       (cl:rational
	1f0)
       (cl:float
	y)
       (qd-real
	y)))
    (single-float
     (etypecase y
       ((or cl:rational single-float)
	x)
       ((or double-float qd-real)
	y)))
    (double-float
     (etypecase y
       ((or cl:rational single-float double-float)
	x)
       (qd-real
	y)))
    (qd-real
     x)))
    
;; Return a floating point (or complex) type of the highest precision
;; among all of the given arguments.
(defun float-contagion (&rest args)
  ;; It would be easy if we could just add the args together and let
  ;; normal contagion do the work, but we could easily introduce
  ;; overflows or other errors that way.  So look at each argument and
  ;; determine the precision and choose the highest precision.  
  (etypecase (reduce #'float-contagion-2 (mapcar #'realpart (if (cdr args)
								args
								(list (car args) 0))))
    (single-float 'single-float)
    (double-float 'double-float)
    (qd-real 'qd-real)))

(defun apply-contagion (number precision)
  (etypecase number
    ((or cl:real qd-real)
     (coerce number precision))
    ((or cl:complex qd-complex)
     (complex (coerce (realpart number) precision)
	      (coerce (imagpart number) precision)))))

;; WITH-FLOATING-POINT-CONTAGION - macro
;;
;; Determines the highest precision of the variables in VARLIST and
;; converts each of the values to that precision.
(defmacro with-floating-point-contagion (varlist &body body)
  (let ((precision (gensym "PRECISION-")))
    `(let ((,precision (float-contagion ,@varlist)))
       (let (,@(mapcar #'(lambda (v)
			   `(,v (apply-contagion ,v ,precision)))
		       varlist))
	 ,@body))))

(defmethod add1 ((a number))
  (cl::1+ a))

(defmethod add1 ((a qd-real))
  (make-instance 'qd-real :value (add-qd-d (qd-value a) 1d0)))

(defmethod sub1 ((a number))
  (cl::1- a))

(defmethod sub1 ((a qd-real))
  (make-instance 'qd-real :value (sub-qd-d (qd-value a) 1d0)))

(declaim (inline 1+ 1-))

(defun 1+ (x)
  (add1 x))

(defun 1- (x)
  (sub1 x))

(defmethod two-arg-+ ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (add-qd (qd-value a) (qd-value b))))

(defmethod two-arg-+ ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (add-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-+ ((a qd-real) (b ext:double-double-float))
  (make-instance 'qd-real :value (add-qd-dd (qd-value a) b)))

(defmethod two-arg-+ ((a real) (b qd-real))
  (+ b a))

(defmethod two-arg-+ ((a number) (b number))
  (cl:+ a b))

(defun + (&rest args)
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-+ res (car args))))
	  ((null args) res))))

(defmethod two-arg-- ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (sub-qd (qd-value a) (qd-value b))))

(defmethod two-arg-- ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (sub-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-- ((a qd-real) (b ext:double-double-float))
  (make-instance 'qd-real :value (sub-qd-dd (qd-value a) b)))

(defmethod two-arg-- ((a cl:float) (b qd-real))
  (make-instance 'qd-real :value (sub-d-qd (cl:float a 1d0) (qd-value b))))

(defmethod two-arg-- ((a number) (b number))
  (cl:- a b))

(defmethod unary-minus ((a number))
  (cl:- a))

(defmethod unary-minus ((a qd-real))
  (make-instance 'qd-real :value (neg-qd (qd-value a))))

(defun - (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-- result (car nlist))))
      (unary-minus number)))


(defmethod two-arg-* ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (mul-qd (qd-value a) (qd-value b))))

(defmethod two-arg-* ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (mul-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-* ((a qd-real) (b ext:double-double-float))
  ;; We'd normally want to use mul-qd-dd, but mul-qd-dd is broken.
  (make-instance 'qd-real :value (mul-qd (qd-value a)
					 (make-qd-dd b 0w0))))

(defmethod two-arg-* ((a real) (b qd-real))
  (* b a))

(defmethod two-arg-* ((a number) (b number))
  (cl:* a b))

(defun * (&rest args)
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-* res (car args))))
	  ((null args) res))))

(defmethod two-arg-/ ((a qd-real) (b qd-real))
  (make-instance 'qd-real :value (div-qd (qd-value a) (qd-value b))))

(defmethod two-arg-/ ((a qd-real) (b cl:float))
  (make-instance 'qd-real :value (div-qd-d (qd-value a) (cl:float b 1d0))))

#+cmu
(defmethod two-arg-/ ((a qd-real) (b ext:double-double-float))
  (make-instance 'qd-real :value (div-qd-dd (qd-value a)
					    b)))

(defmethod two-arg-/ ((a cl:float) (b qd-real))
  (make-instance 'qd-real :value (div-qd (make-qd-d (cl:float a 1d0))
					 (qd-value b))))

#+cmu
(defmethod two-arg-/ ((a ext:double-double-float) (b qd-real))
  (make-instance 'qd-real :value (div-qd (make-qd-dd a 0w0)
					 (qd-value b))))

(defmethod two-arg-/ ((a number) (b number))
  (cl:/ a b))

(defmethod unary-divide ((a number))
  (cl:/ a))

(defmethod unary-divide ((a qd-real))
  (make-instance 'qd-real :value (div-qd +qd-one+ (qd-value a))))

(defun / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-/ result (car nlist))))
      (unary-divide number)))

(macrolet ((frob (name &optional (type 'real))
	     (let ((method-name (intern (concatenate 'string
						     (string '#:q)
						     (symbol-name name))))
		   (cl-name (intern (symbol-name name) :cl))
		   (qd-name (intern (concatenate 'string
						 (symbol-name name)
						 (string '#:-qd)))))
	       `(progn
		  (defmethod ,method-name ((x ,type))
		    (,cl-name x))
		  (defmethod ,method-name ((x qd-real))
		    (,qd-name (qd-value x)))
		  (declaim (inline ,name))
		  (defun ,name (x)
		    (,method-name x))))))
  (frob zerop number)
  (frob plusp)
  (frob minusp))

(defun bignum-to-qd (bignum)
  (make-instance 'qd-real
		 :value (rational-to-qd bignum)))

(defun floatp (x)
  (typep x '(or short-float single-float double-float long-float qd-real)))

(defmethod qfloat ((x real) (num-type cl:float))
  (cl:float x num-type))

(defmethod qfloat ((x cl:float) (num-type qd-real))
  (make-instance 'qd-real :value (make-qd-d (cl:float x 1d0))))

(defmethod qfloat ((x integer) (num-type qd-real))
  (cond ((typep x 'fixnum)
	 (make-instance 'qd-real :value (make-qd-d (cl:float x 1d0))))
	(t
	 ;; A bignum
	 (bignum-to-qd x))))

#+nil
(defmethod qfloat ((x ratio) (num-type qd-real))
  ;; This probably has some issues with roundoff
  (two-arg-/ (qfloat (numerator x) num-type)
	     (qfloat (denominator x) num-type)))

(defmethod qfloat ((x ratio) (num-type qd-real))
  (make-instance 'qd-real :value (rational-to-qd x)))
  
#+cmu
(defmethod qfloat ((x ext:double-double-float) (num-type qd-real))
    (make-instance 'qd-real :value (make-qd-dd x 0w0)))

(defmethod qfloat ((x qd-real) (num-type cl:float))
  (multiple-value-bind (q0 q1 q2 q3)
      (qd-parts (qd-value x))
    (cl:float (cl:+ q0 q1 q2 q3) num-type)))

#+cmu
(defmethod qfloat ((x qd-real) (num-type ext:double-double-float))
  (multiple-value-bind (q0 q1 q2 q3)
      (qd-parts (qd-value x))
    (cl:+ (cl:float q0 1w0)
	  (cl:float q1 1w0)
	  (cl:float q2 1w0)
	  (cl:float q3 1w0))))

(defmethod qfloat ((x qd-real) (num-type qd-real))
  x)

(declaim (inline float))
(defun float (x &optional (other nil otherp))
  (if otherp
      (qfloat x other)
      (if (floatp x)
	  x
	  (qfloat x 1.0))))

(defmethod qrealpart ((x number))
  (cl:realpart x))
(defmethod qrealpart ((x qd-real))
  x)
(defmethod qrealpart ((x qd-complex))
  (make-instance 'qd-real :value (qd-real x)))
(defun realpart (x)
  (qrealpart x))

(defmethod qimagpart ((x number))
  (cl:imagpart x))
(defmethod qimagpart ((x qd-real))
  (make-qd 0d0))
(defmethod qimagpart ((x qd-complex))
  (make-instance 'qd-real :value (qd-imag x)))

(defun imagpart (x)
  (qimagpart x))

(defmethod qconjugate ((a number))
  (cl:conjugate a))

(defmethod qconjugate ((a qd-real))
  (make-instance 'qd-real :value (qd-value a)))

(defmethod qconjugate ((a qd-complex))
  (make-instance 'qd-complex
		 :real (qd-real a)
		 :imag (neg-qd (qd-imag a))))

(defun conjugate (z)
  (qconjugate z))

(defmethod qscale-float ((f cl:float) (n integer))
  (cl:scale-float f n))

(defmethod qscale-float ((f qd-real) (n integer))
  (make-instance 'qd-real :value (scale-float-qd (qd-value f) n)))

(declaim (inline scale-float))
(defun scale-float (f n)
  (qscale-float f n))

(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string
					  (string '#:two-arg-)
					  (symbol-name op))))
	     (cl-fun (find-symbol (symbol-name op) :cl))
	     (qd-fun (intern (concatenate 'string (string '#:qd-) (symbol-name op))
			     '#:octi)))
	 `(progn
	    (defmethod ,method ((a real) (b real))
	      (,cl-fun a b))
	    (defmethod ,method ((a qd-real) (b real))
	      (,qd-fun (qd-value a) (make-qd-d (cl:float b 1d0))))
	    (defmethod ,method ((a real) (b qd-real))
	      ;; This is not really right if A is a rational.  We're
	      ;; supposed to compare them as rationals.
	      (,qd-fun (make-qd-d (cl:float a 1d0)) (qd-value b)))
	    (defmethod ,method ((a qd-real) (b qd-real))
	      (,qd-fun (qd-value a) (qd-value b)))
	    (defun ,op (number &rest more-numbers)
	      "Returns T if its arguments are in strictly increasing order, NIL otherwise."
	      (declare (optimize (safety 2))
		       (dynamic-extent more-numbers))
	      (do* ((n number (car nlist))
		    (nlist more-numbers (cdr nlist)))
		   ((atom nlist) t)
		(declare (list nlist))
		(if (not (,method n (car nlist))) (return nil))))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

;; Handle the special functions for a real argument.  Complex args are
;; handled elsewhere.
(macrolet
    ((frob (name)
       (let ((method-name
	      (intern (concatenate 'string (string '#:q)
				   (symbol-name name))))
	     (cl-name (intern (symbol-name name) :cl))
	     (qd-name (intern (concatenate 'string (symbol-name name)
					   (string '#:-qd)))))
	 `(progn
	    (defmethod ,name ((x number))
	      (,cl-name x))
	    (defmethod ,name ((x qd-real))
	      (make-instance 'qd-real :value (,qd-name (qd-value x))))))))
  (frob abs)
  (frob exp)
  (frob sin)
  (frob cos)
  (frob tan)
  ;;(frob asin)
  ;;(frob acos)
  (frob sinh)
  (frob cosh)
  (frob tanh)
  (frob asinh)
  ;;(frob acosh)
  ;;(frob atanh)
  )

(defmethod sqrt ((x number))
  (cl:sqrt x))

(defmethod sqrt ((x qd-real))
  (if (minusp x)
      (make-instance 'qd-complex
		     :real +qd-zero+
		     :imag (sqrt-qd (neg-qd (qd-value x))))
      (make-instance 'qd-real :value (sqrt-qd (qd-value x)))))

(defun scalb (x n)
  "Compute 2^N * X without compute 2^N first (use properties of the
underlying floating-point format"
  (declare (type qd-real x))
  (scale-float x n))

(declaim (inline qd-cssqs))
(defun qd-cssqs (z)
  (multiple-value-bind (rho k)
      (octi::hypot-aux-qd (qd-value (realpart z))
			 (qd-value (imagpart z)))
    (values (make-instance 'qd-real :value rho)
	    k)))

#+nil
(defmethod qabs ((z qd-complex))
  ;; sqrt(x^2+y^2)
  ;; If |x| > |y| then sqrt(x^2+y^2) = |x|*sqrt(1+(y/x)^2)
  (multiple-value-bind (abs^2 rho)
      (hypot-qd (qd-value (realpart z))
		(qd-value (imagpart z)))
    (scale-float (make-instance 'qd-real :value (sqrt abs^2))
		 rho)))

(defmethod abs ((z qd-complex))
  ;; sqrt(x^2+y^2)
  ;; If |x| > |y| then sqrt(x^2+y^2) = |x|*sqrt(1+(y/x)^2)
  (make-instance 'qd-real
		 :value (hypot-qd (qd-value (realpart z))
				  (qd-value (imagpart z)))))

(defmethod log ((a number) &optional b)
  (if b
      (cl:log a b)
      (cl:log a)))

(defmethod log ((a qd-real) &optional b)
  (if b
      (/ (log a) (log b))
      (if (minusp (float-sign a))
	  (make-instance 'qd-complex
			 :real (log-qd (abs-qd (qd-value a)))
			 :imag +qd-pi+)
	  (make-instance 'qd-real :value (log-qd (qd-value a))))))

(defmethod log1p ((a qd-real))
  (make-instance 'qd-real :value (log1p-qd (qd-value a))))

(defmethod atan ((y real) &optional x)
  (cond (x
	 (cond ((typep x 'qd-real)
		(make-instance 'qd-real
			       :value (atan2-qd (qd-value y) (qd-value x))))
	       (t
		(cl:atan y x))))
	(t
	 (cl:atan y))))

(defmethod atan ((y qd-real) &optional x)
  (make-instance 'qd-real
		 :value
		 (if x
		     (atan2-qd (qd-value y) (qd-value x))
		     (atan-qd (qd-value y)))))

(defmethod qexpt ((x number) (y number))
  (cl:expt x y))

(defmethod qexpt ((x number) (y qd-real))
  (exp (* y (log (apply-contagion x 'qd-real)))))

(defmethod qexpt ((x number) (y qd-complex))
  (exp (* y (log (apply-contagion x 'qd-real)))))

(defmethod qexpt ((x qd-real) (y real))
  (exp (* y (log x))))

(defmethod qexpt ((x qd-real) (y cl:complex))
  (exp (* (make-instance 'qd-complex
			 :real (qd-value (realpart y))
			 :imag (qd-value (imagpart y)))
	  (log x))))

(defmethod qexpt ((x qd-real) (y qd-real))
  ;; x^y = exp(y*log(x))
  (exp (* y (log x))))

(defmethod qexpt ((x qd-real) (y integer))
  (make-instance 'qd-real
		 :value (npow (qd-value x) y)))

(declaim (inline expt))
(defun expt (x y)
  (qexpt x y))



(defmethod two-arg-= ((a number) (b number))
  (cl:= a b))

(defmethod two-arg-= ((a qd-real) (b number))
  (if (cl:realp b)
      (qd-= (qd-value a) (make-qd-d (cl:float b 1d0)))
      nil))

(defmethod two-arg-= ((a number) (b qd-real))
  (if (cl:realp a)
      (qd-= (make-qd-d (cl:float a 1d0)) (qd-value b))
      nil))

(defmethod two-arg-= ((a qd-complex) b)
  (and (two-arg-= (realpart a) (realpart b))
       (two-arg-= (imagpart a) (imagpart b))))

(defmethod two-arg-= (a (b qd-complex))
  (and (two-arg-= (realpart a) (realpart b))
       (two-arg-= (imagpart a) (imagpart b))))


(defmethod two-arg-= ((a qd-real) (b qd-real))
  (qd-= (qd-value a) (qd-value b)))

(defun = (number &rest more-numbers)
  "Returns T if all of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist)))
      ((atom nlist) t)
    (declare (list nlist))
    (if (not (two-arg-= (car nlist) number))
	(return nil))))

(defun /= (number &rest more-numbers)
  "Returns T if no two of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   (dynamic-extent more-numbers))
  (do* ((head number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
    (declare (list nlist))
    (unless (do* ((nl nlist (cdr nl)))
		 ((atom nl) t)
	      (declare (list nl))
	      (if (two-arg-= head (car nl))
		  (return nil)))
      (return nil))))

(defmethod qcomplex ((x cl:real) (y cl:real))
  (cl:complex x y))

(defmethod qcomplex ((x cl:real) (y qd-real))
  (qcomplex (make-qd x) y))

(defmethod qcomplex ((x qd-real) (y qd-real))
  (make-instance 'qd-complex
		 :real (qd-value x)
		 :imag (qd-value y)))

(defmethod qcomplex ((x qd-real) (y cl:real))
  (make-instance 'qd-complex
		 :real (qd-value x)
		 :imag (make-qd-d y)))

(defun complex (x &optional (y 0))
  (qcomplex x y))

(defmethod qinteger-decode-float ((f cl:float))
  (cl:integer-decode-float f))

(defmethod qinteger-decode-float ((f qd-real))
  (integer-decode-qd (qd-value f)))

(declaim (inline integer-decode-float))
(defun integer-decode-float (f)
  (qinteger-decode-float f))

(defmethod qdecode-float ((f cl:float))
  (cl:decode-float f))

(defmethod qdecode-float ((f qd-real))
  (multiple-value-bind (frac exp s)
      (decode-float-qd (qd-value f))
    (values (make-instance 'qd-real :value frac)
	    exp
	    (make-instance 'qd-real :value  s))))

(declaim (inline decode-float))
(defun decode-float (f)
  (qdecode-float f))

(defmethod qfloor ((x real) &optional y)
  (if y
      (cl:floor x y)
      (cl:floor x)))

(defmethod qfloor ((x qd-real) &optional y)
  (if (and y (/= y 1))
      (let ((f (qfloor (/ x y))))
	(values f
		(- x (* f y))))
      (let ((f (ffloor-qd (qd-value x))))
	(multiple-value-bind (int exp sign)
	    (integer-decode-qd f)
	  (values (ash (* sign int) exp)
		  (make-instance 'qd-real
				 :value (qd-value
					 (- x (make-instance 'qd-real
							     :value f)))))))))

(defun floor (x &optional y)
  (qfloor x y))

(defmethod qffloor ((x real) &optional y)
  (if y
      (cl:ffloor x y)
      (cl:ffloor x)))

(defmethod qffloor ((x qd-real) &optional y)
  (if (and y (/= y 1))
      (let ((f (qffloor (/ x y))))
	(values f
		(- x (* f y))))
      (let ((f (make-instance 'qd-real :value (ffloor-qd (qd-value x)))))
	(values f
		(- x f)))))

(defun ffloor (x &optional y)
  (qffloor x y))

(defun ceiling (x &optional y)
  (multiple-value-bind (f rem)
      (floor x y)
    (if (zerop rem)
	(values f
		rem)
	(values (+ f 1)
		(- rem 1)))))

(defun fceiling (x &optional y)
  (multiple-value-bind (f rem)
      (ffloor x y)
    (if (zerop rem)
	(values f
		rem)
	(values (+ f 1)
		(- rem 1)))))

(defun truncate (x &optional (y 1))
  (if (minusp x)
      (ceiling x y)
      (floor x y)))

(defun rem (x y)
  (nth-value 1 (truncate x y)))

(defun mod (x y)
  (nth-value 1 (floor x y)))

(defun ftruncate (x &optional (y 1))
  (if (minusp x)
      (fceiling x y)
      (ffloor x y)))

(defmethod %unary-round ((x real))
  (cl::round x))

(defmethod %unary-round ((number qd-real))
  (multiple-value-bind (bits exp)
      (integer-decode-float number)
    (let* ((shifted (ash bits exp))
	   (rounded (if (and (minusp exp)
			     (oddp shifted)
			     (not (zerop (logand bits
						 (ash 1 (- -1 exp))))))
			(1+ shifted)
			shifted)))
      (if (minusp number)
	  (- rounded)
	  rounded))))

(defun round (number &optional (divisor 1))
  (if (eql divisor 1)
      (let ((r (%unary-round number)))
	(values r
		(- number r)))
      (multiple-value-bind (tru rem)
	  (truncate number divisor)
	(if (zerop rem)
	    (values tru rem)
	    (let ((thresh (/ (abs divisor) 2)))
	      (cond ((or (> rem thresh)
			 (and (= rem thresh) (oddp tru)))
		     (if (minusp divisor)
			 (values (- tru 1) (+ rem divisor))
			 (values (+ tru 1) (- rem divisor))))
		    ((let ((-thresh (- thresh)))
		       (or (< rem -thresh)
			   (and (= rem -thresh) (oddp tru))))
		     (if (minusp divisor)
			 (values (+ tru 1) (- rem divisor))
			 (values (- tru 1) (+ rem divisor))))
		    (t (values tru rem))))))))

(defun fround (number &optional (divisor 1))
  "Same as ROUND, but returns first value as a float."
  (multiple-value-bind (res rem)
      (round number divisor)
    (values (float res (if (floatp rem) rem 1.0)) rem)))

(defmethod qfloat-sign ((a real) &optional (f (float 1 a)))
  (cl:float-sign a f))


(defmethod qfloat-sign ((a qd-real) &optional f)
  (if f
      (make-instance 'qd-real
		     :value (mul-qd-d (abs-qd (qd-value f))
				      (cl:float-sign (qd-0 (qd-value a)))))
      (make-instance 'qd-real :value (make-qd-d (cl:float-sign (qd-0 (qd-value a)))))))

(declaim (inline float-sign))
(defun float-sign (n &optional (float2 nil float2p))
  (if float2p
      (qfloat-sign n float2)
      (qfloat-sign n)))

(defun max (number &rest more-numbers)
  "Returns the greatest of its arguments."
  (declare (optimize (safety 2)) (type (or real qd-real) number)
	   (dynamic-extent more-numbers))
  (dolist (real more-numbers)
    (when (> real number)
      (setq number real)))
  number)

(defun min (number &rest more-numbers)
  "Returns the least of its arguments."
  (declare (optimize (safety 2)) (type (or real qd-real) number)
	   (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist))
       (result (the (or real qd-real) number)))
      ((null nlist) (return result))
    (declare (list nlist))
    (if (< (car nlist) result)
	(setq result (car nlist)))))

(defmethod asin ((x number))
  (cl:asin x))

(defmethod asin ((x qd-real))
  (if (<= -1 x 1)
      (make-instance 'qd-real :value (asin-qd (qd-value x)))
      (qd-complex-asin x)))

(defmethod acos ((x number))
  (cl:acos x))

(defmethod acos ((x qd-real))
  (cond ((> (abs x) 1)
	 (qd-complex-acos x))
	(t
	 (make-instance 'qd-real :value (acos-qd (qd-value x))))))

(defmethod acosh ((x number))
  (cl:acosh x))

(defmethod acosh ((x qd-real))
  (if (< x 1)
      (qd-complex-acosh x)
      (make-instance 'qd-real :value (acosh-qd (qd-value x)))))

(defmethod atanh ((x number))
  (cl:atanh x))

(defmethod atanh ((x qd-real))
  (if (> (abs x) 1)
      (qd-complex-atanh x)
      (make-instance 'qd-real :value (atanh-qd (qd-value x)))))

(defmethod cis ((x real))
  (cl:cis x))

(defmethod cis ((x qd-real))
  (multiple-value-bind (s c)
      (sincos-qd (qd-value x))
    (make-instance 'qd-complex
		   :real c
		   :imag s)))

(defmethod phase ((x number))
  (cl:phase x))

(defmethod phase ((x qd-real))
  (if (minusp x)
      (- +pi+)
      (make-instance 'qd-real :value (make-qd-d 0d0))))

(defun signum (number)
  "If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER))."
  (if (zerop number)
      number
      (if (rationalp number)
	  (if (plusp number) 1 -1)
	  (/ number (abs number)))))

(defmethod random ((x cl:real) &optional (state *random-state*))
  (cl:random x state))

(defmethod random ((x qd-real) &optional (state *random-state*))
  (* x (make-instance 'qd-real
		      :value (octi:random-qd state))))

(defmethod float-digits ((x cl:real))
  (cl:float-digits x))

(defmethod float-digits ((x qd-real))
  (* 4 (float-digits 1d0)))

(defmethod rational ((x real))
  (cl:rational x))

(defmethod rational ((x qd-real))
  (with-qd-parts (x0 x1 x2 x3)
      (qd-value x)
    (+ (cl:rational x0)
       (cl:rational x1)
       (cl:rational x2)
       (cl:rational x3))))

(defmethod rationalize ((x cl:real))
  (cl:rationalize x))

;;; The algorithm here is the method described in CLISP.  Bruno Haible has
;;; graciously given permission to use this algorithm.  He says, "You can use
;;; it, if you present the following explanation of the algorithm."
;;;
;;; Algorithm (recursively presented):
;;;   If x is a rational number, return x.
;;;   If x = 0.0, return 0.
;;;   If x < 0.0, return (- (rationalize (- x))).
;;;   If x > 0.0:
;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
;;;     exponent, sign).
;;;     If m = 0 or e >= 0: return x = m*2^e.
;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
;;;     with smallest possible numerator and denominator.
;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
;;;       But in this case the result will be x itself anyway, regardless of
;;;       the choice of a. Therefore we can simply ignore this case.
;;;     Note 2: At first, we need to consider the closed interval [a,b].
;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
;;;       interval (a,b).
;;;     So, for given a and b (0 < a < b) we are searching a rational number
;;;     y with a <= y <= b.
;;;     Recursive algorithm fraction_between(a,b):
;;;       c := (ceiling a)
;;;       if c < b
;;;         then return c       ; because a <= c < b, c integer
;;;         else
;;;           ; a is not integer (otherwise we would have had c = a < b)
;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
;;;
;;; You can see that we are actually computing a continued fraction expansion.
;;;
;;; Algorithm (iterative):
;;;   If x is rational, return x.
;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
;;;     exponent, sign).
;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
;;;   (positive and already in lowest terms because the denominator is a
;;;   power of two and the numerator is odd).
;;;   Start a continued fraction expansion
;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
;;;   Loop
;;;     c := (ceiling a)
;;;     if c >= b
;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
;;;            goto Loop
;;;   finally partial_quotient(c).
;;;   Here partial_quotient(c) denotes the iteration
;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
;;;   At the end, return s * (p[i]/q[i]).
;;;   This rational number is already in lowest terms because
;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
;;;
(defmethod rationalize ((x qd-real))
  ;; This is a fairly straigtforward implementation of the iterative
  ;; algorithm above.
  (multiple-value-bind (frac expo sign)
      (integer-decode-float x)
    (cond ((or (zerop frac) (>= expo 0))
	   (if (minusp sign)
	       (- (ash frac expo))
	       (ash frac expo)))
	  (t
	   ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
	   ;; so build the fraction up immediately, without having to do
	   ;; a gcd.
	   (let ((a (/ (- (* 2 frac) 1) (ash 1 (- 1 expo))))
		 (b (/ (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
		 (p0 0)
		 (q0 1)
		 (p1 1)
		 (q1 0))
	     (do ((c (ceiling a) (ceiling a)))
		 ((< c b)
		  (let ((top (+ (* c p1) p0))
			(bot (+ (* c q1) q0)))
		    (/ (if (minusp sign)
			   (- top)
			   top)
		       bot)))
	       (let* ((k (- c 1))
		      (p2 (+ (* k p1) p0))
		      (q2 (+ (* k q1) q0)))
		 (psetf a (/ (- b k))
			b (/ (- a k)))
		 (setf p0 p1
		       q0 q1
		       p1 p2
		       q1 q2))))))))

(define-compiler-macro + (&whole form &rest args)
  (declare (ignore form))
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-+ ,res ,(car args))))
	  ((null args) res))))

(define-compiler-macro - (&whole form number &rest more-numbers)
  (declare (ignore form))
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-- ,result ,(car nlist))))
      `(unary-minus ,number)))

(define-compiler-macro * (&whole form &rest args)
  (declare (ignore form))
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-* ,res ,(car args))))
	  ((null args) res))))

(define-compiler-macro / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-/ ,result ,(car nlist))))
      `(unary-divide ,number)))

;; Compiler macros to convert <, >, <=, and >= into multiple calls of
;; the corresponding two-arg-<foo> function.
(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string
					  (string '#:two-arg-)
					  (symbol-name op)))))
	 `(define-compiler-macro ,op (number &rest more-numbers)
	    (do* ((n number (car nlist))
		  (nlist more-numbers (cdr nlist))
		  (res nil))
		 ((atom nlist) 
		  `(and ,@(nreverse res)))
	      (push `(,',method ,n ,(car nlist)) res))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

(define-compiler-macro /= (&whole form number &rest more-numbers)
  ;; Convert (/= x y) to (not (two-arg-= x y)).  Should we try to
  ;; handle a few more cases?
  (if (cdr more-numbers)
      form
      `(not (two-arg-= ,number ,(car more-numbers)))))


;; Define compiler macro the convert two-arg-foo into the appropriate
;; CL function or QD-REAL function so we don't have to do CLOS
;; dispatch.
#+(or)
(macrolet
    ((frob (name cl-op qd-op)
       `(define-compiler-macro ,name (&whole form x y &environment env)
	  (flet ((arg-type (arg)
		   (multiple-value-bind (def-type localp decl)
		       (ext:variable-information arg env)
		     (declare (ignore localp))
		     (when def-type
		       (cdr (assoc 'type decl))))))
	    (let ((x-type (arg-type x))
		  (y-type (arg-type y)))
	      (cond ((and (subtypep x-type 'cl:number)
			  (subtypep y-type 'cl:number))
		     `(,',cl-op ,x ,y))
		    ((and (subtypep x-type 'qd-real)
			  (subtypep y-type 'qd-real))
		     `(make-instance 'qd-real :value (,',qd-op (qd-value ,x)
							       (qd-value ,y))))
		    (t
		     ;; Don't know how to handle this, so give up.
		     form)))))))
  (frob two-arg-+ cl:+ add-qd)
  (frob two-arg-- cl:- sub-qd)
  (frob two-arg-* cl:* mul-qd)
  (frob two-arg-/ cl:/ div-qd))

#+(or)
(macrolet
    ((frob (name cl-op qd-op cl-qd-op qd-cl-op)
       `(define-compiler-macro ,name (&whole form x y &environment env)
	  (flet ((arg-type (arg)
		   (multiple-value-bind (def-type localp decl)
		       (ext:variable-information arg env)
		     (declare (ignore localp))
		     (when def-type
		       (cdr (assoc 'type decl))))))
	    (let ((x-type (arg-type x))
		  (y-type (arg-type y)))
	      (cond ((subtypep x-type 'cl:float)
		     (cond ((subtypep y-type 'cl:number)
			    `(,',cl-op ,x ,y))
			   ((subtypep y-type 'qd-real)
			    (if ,cl-qd-op
				`(make-instance 'qd-real :value (,',cl-qd-op (cl:float ,x 1d0)
									     (qd-value ,y)))
				form))
			   (t form)))
		    ((subtypep x-type 'qd-real)
		     (cond ((subtypep y-type 'cl:float)
			    (if ,qd-cl-op
				`(make-instance 'qd-real :value (,',qd-cl-op (qd-value ,x)
									     (float ,y 1d0)))
				form))
			   ((subtypep y-type 'qd-real)
			    `(make-instance 'qd-real :value (,',qd-op (qd-value ,x)
								      (qd-value ,y))))
			   (t form)))
		    (t
		     ;; Don't know how to handle this, so give up.
		     form)))))))
  (frob two-arg-+ cl:+ add-qd add-d-qd add-qd-d)
  (frob two-arg-- cl:- sub-qd sub-d-qd sub-qd-d)
  (frob two-arg-* cl:* mul-qd mul-d-qd mul-qd-d)
  (frob two-arg-/ cl:/ div-qd nil nil))

(defgeneric epsilon (m)
  (:documentation 
"Return an epsilon value of the same precision as the argument.  It is
the smallest number x such that 1+x /= x.  The argument can be
complex"))

(defmethod epsilon ((m cl:float))
  (etypecase m
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)))

(defmethod epsilon ((m cl:complex))
  (epsilon (realpart m)))

(defmethod epsilon ((m qd-real))
  ;; What is the epsilon value for a quad-double?  This is complicated
  ;; by the fact that things like (+ #q1 #q1q-100) is representable as
  ;; a quad-double.  For most purposes we want epsilon to be close to
  ;; the 212 bits of precision (4*53 bits) that we normally have with
  ;; a quad-double.
  (scale-float +qd-real-one+ -212))

(defmethod epsilon ((m qd-complex))
  (epsilon (realpart m)))

(defgeneric float-pi (x)
  (:documentation 
"Return a floating-point value of the mathematical constant pi that is
the same precision as the argument.  The argument can be complex."))

(defmethod float-pi ((x cl:rational))
  (float pi 1f0))

(defmethod float-pi ((x cl:float))
  (float pi x))

(defmethod float-pi ((x qd-real))
  +pi+)

(defmethod float-pi ((z cl:complex))
  (float pi (realpart z)))

(defmethod float-pi ((z qd-complex))
  +pi+)

(defmethod float-nan-p ((x cl:float))
  ;; CMUCL has ext:float-nan-p.  Should we use that instead?
  (not (= x x)))

(defmethod float-nan-p ((x qd-real))
  (float-nan-p (qd-parts (qd-value x))))


(define-condition domain-error (simple-error)
  ((function-name :accessor condition-function-name
		  :initarg :function-name))
  (:report (lambda (condition stream)
	     (format stream "Domain Error for function ~S:~&"
		     (condition-function-name condition))
	     (pprint-logical-block (stream nil :per-line-prefix "  ")
	       (apply #'format stream
		      (simple-condition-format-control condition)
		      (simple-condition-format-arguments condition))))))
