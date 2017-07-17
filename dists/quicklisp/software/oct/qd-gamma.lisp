;;;; -*- Mode: lisp -*-
;;;;
;;;; Copyright (c) 2011 Raymond Toy
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* *oct-readtable*))

;; For log-gamma we use the asymptotic formula
;;
;; log(gamma(z)) ~ (z - 1/2)*log(z) + log(2*%pi)/2
;;                   + sum(bern(2*k)/(2*k)/(2*k-1)/z^(2k-1), k, 1, inf)
;;
;;               = (z - 1/2)*log(z) + log(2*%pi)/2
;;                  + 1/12/z*(1 - 1/30/z^2 + 1/105/z^4 + 1/140/z^6 + ...
;;                              + 174611/10450/z^18 + ...)
;;
;; For double-floats, let's stop the series at the power z^18.  The
;; next term is 77683/483/z^20.  This means that for |z| > 8.09438,
;; the series has double-float precision.
;;
;; For quad-doubles, let's stop the series at the power z^62.  The
;; next term is about 6.364d37/z^64.  So for |z| > 38.71, the series
;; has quad-double precision.
(defparameter *log-gamma-asymp-coef*
  #(-1/30 1/105 -1/140 1/99 -691/30030 1/13 -3617/10200 43867/20349 
    -174611/10450 77683/483 -236364091/125580 657931/25 -3392780147/7830 
    1723168255201/207669 -7709321041217/42160 151628697551/33 
    -26315271553053477373/201514950 154210205991661/37 
    -261082718496449122051/1758900 1520097643918070802691/259161 
    -2530297234481911294093/9890 25932657025822267968607/2115 
    -5609403368997817686249127547/8725080 19802288209643185928499101/539 
    -61628132164268458257532691681/27030 29149963634884862421418123812691/190323 
    -354198989901889536240773677094747/31900 
    2913228046513104891794716413587449/3363 
    -1215233140483755572040304994079820246041491/16752085350 
    396793078518930920708162576045270521/61 
    -106783830147866529886385444979142647942017/171360 
    133872729284212332186510857141084758385627191/2103465
    ))

#+nil
(defun log-gamma-asymp-series (z nterms)
  ;; Sum the asymptotic formula for n terms
  ;;
  ;; 1 + sum(c[k]/z^(2*k+2), k, 0, nterms)
  (let ((z2 (* z z))
	(sum 1)
	(term 1))
    (dotimes (k nterms)
      (setf term (* term z2))
      (incf sum (/ (aref *log-gamma-asymp-coef* k) term)))
    sum))

(defun log-gamma-asymp-series (z nterms)
  (loop with y = (* z z)
     for k from 1 to nterms
     for x = 0 then
       (setf x (/ (+ x (aref *log-gamma-asymp-coef* (- nterms k)))
		  y))
     finally (return (+ 1 x))))
       

(defun log-gamma-asymp-principal (z nterms log2pi/2)
  (+ (- (* (- z 1/2)
	   (log z))
	z)
     log2pi/2))

(defun log-gamma-asymp (z nterms log2pi/2)
  (+ (log-gamma-asymp-principal z nterms log2pi/2)
     (* 1/12 (/ (log-gamma-asymp-series z nterms) z))))

(defun log2pi/2 (precision)
  (ecase precision
    (single-float
     (coerce (/ (log (* 2 pi)) 2) 'single-float))
    (double-float
     (coerce (/ (log (* 2 pi)) 2) 'double-float))
    (qd-real
     (/ (log +2pi+) 2))))

(defun log-gamma-aux (z limit nterms)
  (let ((precision (float-contagion z)))
    (cond ((minusp (realpart z))
	   ;; Use reflection formula if realpart(z) < 0
	   ;;   log(gamma(-z)) = log(pi)-log(-z)-log(sin(pi*z))-log(gamma(z))
	   ;; Or
	   ;;   log(gamma(z)) = log(pi)-log(-z)-log(sin(pi*z))-log(gamma(-z))
	   (let ((p (float-pi z)))
	     (- (log p)
		(log (- z))
		(log (sin (* p z)))
		(log-gamma (- z)))))
	  (t
	   (let ((absz (abs z)))
	     (cond ((>= absz limit)
		    ;; Can use the asymptotic formula directly with 9 terms
		    (log-gamma-asymp z nterms (log2pi/2 precision)))
		   (t
		    ;; |z| is too small.  Use the formula
		    ;; log(gamma(z)) = log(gamma(z+1)) - log(z)
		    (- (log-gamma (+ z 1))
		       (log z)))))))))

(defmethod log-gamma ((z cl:number))
  (log-gamma-aux z 9 9))

(defmethod log-gamma ((z qd-real))
  (log-gamma-aux z 26 26))

(defmethod log-gamma ((z qd-complex))
  (log-gamma-aux z 26 26))

(defun gamma-aux (z limit nterms)
  (let ((precision (float-contagion z)))
    (cond ((<= (realpart z) 0)
	   ;; Use reflection formula if realpart(z) < 0:
	   ;;  gamma(-z) = -pi*csc(pi*z)/gamma(z+1)
	   ;; or
	   ;;  gamma(z) = pi*csc(pi*z)/gamma(1-z)
	   (if (and (realp z)
		    (= (truncate z) z))
	       ;; Gamma of a negative integer is infinity.  Signal an error
	       (error "Gamma of non-positive integer ~S" z)
	       (/ (float-pi z)
		  (sin (* (float-pi z) z))
		  (gamma-aux (- 1 z) limit nterms))))
	  ((and (zerop (imagpart z))
		(= z (truncate z)))
	   ;; We have gamma(n) where an integer value n and is small
	   ;; enough.  In this case, just compute the product
	   ;; directly.  We do this because our current implementation
	   ;; has some round-off for these values, and that's annoying
	   ;; and unexpected.
	   (let ((n (truncate z)))
	     (loop
		for prod = (apply-contagion 1 precision) then (* prod k)
		for k from 2 below n
		finally (return (apply-contagion prod precision)))))
	  (t
	   (let ((absz (abs z)))
	     (cond ((>= absz limit)
		    ;; Use log gamma directly:
		    ;;  log(gamma(z)) = principal part + 1/12/z*(series part)
		    ;; so
		    ;;  gamma(z) = exp(principal part)*exp(1/12/z*series)
		    (exp (log-gamma z))
		    #+nil
		    (* (exp (log-gamma-asymp-principal z nterms
						       (log2pi/2 precision)))
		       (exp (* 1/12 (/ (log-gamma-asymp-series z nterms) z)))))
		   (t
		    ;; 1 <= |z| <= limit
		    ;; gamma(z) = gamma(z+1)/z
		    (/ (gamma-aux (+ 1 z) limit nterms) z))))))))
		 
(defmethod gamma ((z cl:number))
  (gamma-aux z 9 9))

(defmethod gamma ((z qd-real))
  (gamma-aux z 39 32))

(defmethod gamma ((z qd-complex))
  (gamma-aux z 39 32))

;; Lentz's algorithm for evaluating continued fractions.
;;
;; Let the continued fraction be:
;;
;;      a1    a2    a3
;; b0 + ----  ----  ----
;;      b1 +  b2 +  b3 +
;;

(defvar *debug-cf-eval*
  nil
  "When true, enable some debugging prints when evaluating a
  continued fraction.")

;; Max number of iterations allowed when evaluating the continued
;; fraction.  When this is reached, we assume that the continued
;; fraction did not converge.
(defvar *max-cf-iterations*
  10000
  "Max number of iterations allowed when evaluating the continued
  fraction.  When this is reached, we assume that the continued
  fraction did not converge.")

(defun lentz (bf af)
  (let ((tiny-value-count 0))
    (flet ((value-or-tiny (v)
	     (if (zerop v)
		 (progn
		   (incf tiny-value-count)
		   (etypecase v
		     ((or double-float cl:complex)
		      (sqrt least-positive-normalized-double-float))
		     ((or qd-real qd-complex)
		      (make-qd (sqrt least-positive-normalized-double-float)))))
		 v)))
      (let* ((f (value-or-tiny (funcall bf 0)))
	     (c f)
	     (d 0)
	     (eps (epsilon f)))
	(loop
	   for j from 1 upto *max-cf-iterations*
	   for an = (funcall af j)
	   for bn = (funcall bf j)
	   do (progn
		(setf d (value-or-tiny (+ bn (* an d))))
		(setf c (value-or-tiny (+ bn (/ an c))))
		(when *debug-cf-eval*
		  (format t "~&j = ~d~%" j)
		  (format t "  an = ~s~%" an)
		  (format t "  bn = ~s~%" bn)
		  (format t "  c  = ~s~%" c)
		  (format t "  d  = ~s~%" d))
		(let ((delta (/ c d)))
		  (setf d (/ d))
		  (setf f (* f delta))
		  (when *debug-cf-eval*
		    (format t "  dl= ~S (|dl - 1| = ~S)~%" delta (abs (1- delta)))
		    (format t "  f = ~S~%" f))
		  (when (<= (abs (- delta 1)) eps)
		    (return-from lentz (values f j tiny-value-count)))))
	   finally
	     (error 'simple-error
		    :format-control "~<Continued fraction failed to converge after ~D iterations.~%    Delta = ~S~>"
		    :format-arguments (list *max-cf-iterations* (/ c d))))))))

;; Continued fraction for erf(z):
;;
;;   erf(z) = 2*z/sqrt(pi)*exp(-z^2)/K
;;
;; where K is the continued fraction with
;;
;;   b[n] = 1+2*n-2*z^2
;;   a[n] = 4*n*z^2
;;
;; This works ok, but has problems for z > 3 where sometimes the
;; result is greater than 1 and for larger values, negative numbers
;; are returned.
#+nil
(defun cf-erf (z)
  (let* ((z2 (* z z))
	 (twoz2 (* 2 z2)))
    (* (/ (* 2 z)
	  (sqrt (float-pi z)))
       (exp (- z2))
       (/ (lentz #'(lambda (n)
		     (- (+ 1 (* 2 n))
			twoz2))
		 #'(lambda (n)
		     (* 4 n z2)))))))

;; From Cuyt
;;
;; sqrt(%pi)*z*exp(z^2)*erf(z) = K
;;
;; where K is the continued fraction with terms F[m]*z^2/(1 + G[m]*z^2)
;; with F[1] = 2 and F[m] = 4*(m-1)/(2*m-3)/(2*m-1) and G[m] = -2/(2*m-1)
;;
(defun cf-erf (z)
  (let ((z2 (* z z)))
    (* (/ (exp (- z2))
	  (sqrt (float-pi z))
	  z)
       (lentz #'(lambda (n)
		  (if (zerop n)
		      (float 0 (realpart z))
		      (1+ (/ (* -2 z2)
			     (+ n n -1)))))
	      #'(lambda (n)
		  (if (= n 1)
		      (* 2 z2)
		      (/ (* z2 4 (- n 1))
			 (* (+ n n -3) (+ n n -1)))))))))

;; From the above, we also have Dawson's integral:
;;
;; exp(-z^-2)*integrate(exp(t^2), t, 0, z) = %i*sqrt(%pi)/2*exp(-z^2)*erf(-%i*z);
;;
;; -2*z*exp(-z^2)*integrate(exp(t^2), t, 0, z) = K
;;
;; with K = -F[m)*z^2/(1 - G[m]*z^2), where F[m] and G[m] are as above.
;;
;; Also erf(-%i*z) = dawson(z) * 2*exp(-z^2)/(*%i*sqrt(%pi))
(defun cf-dawson (z)
  (let ((z2 (* z z)))
    (/ (lentz #'(lambda (n)
		  (if (zerop n)
		      (float 0 (realpart z))
		      (- 1 (/ (* -2 z2)
			      (+ n n -1)))))
	      #'(lambda (n)
		  (if (= n 1)
		      (* -2 z2)
		      (/ (* z2 -4 (- n 1))
			 (* (+ n n -3) (+ n n -1))))))
       (* -2 z))))

;; erfc(z) = z/sqrt(%pi)*exp(-z^2)*K
;;
;; where K is the continued fraction with a[1] = 1, a[m] = (m-1)/2,
;; for m >= 2 and b[0] = 0, b[2*m+1] = z^2, b[2*m] = 1.
;;
;; This is valid only if Re(z) > 0.

(defun cf-erfc (z)
  (let ((z2 (* z z))
	(zero (float 0 (realpart z)))
	(one (float 1 (realpart z))))
    (* (exp (- z2))
       z
       (/ (sqrt (float-pi (realpart z))))
       (lentz #'(lambda (n)
		  (if (zerop n)
		      zero
		      (if (evenp n)
			  one
			  z2)))
	      #'(lambda (n)
		  (if (= n 1)
		      one
		      (/ (- n 1) 2)))))))

;; w(z) = exp(-z^2)*erfc(-%i*z)
;;
;;      = -%i*z/sqrt(%pi)*K
;;
;; where K is the continued fraction with a[n] the same as for erfc
;; and b[0] = 0, b[2*m+1] = -z^2, b[2*m] = 1.
;;
;; This is valid only if Im(z) > 0.  We can use the following
;; identities:
;;
;; w(-z) = 2*exp(-z^2) - w(z)
;; w(conj(z)) = conj(w(-z))

(defun cf-w (z)
  (let ((z2 (* z z))
	(zero (float 0 (realpart z)))
	(one (float 1 (realpart z))))
    (* #c(0 -1)
       z
       (/ (sqrt (float-pi (realpart z))))
       (lentz #'(lambda (n)
		  (if (zerop n)
		      zero
		      (if (evenp n)
			  one
			  (- z2))))
	      #'(lambda (n)
		  (if (= n 1)
		      one
		      (/ (- n 1) 2)))))))
;; Tail of the incomplete gamma function:
;; integrate(x^(a-1)*exp(-x), x, z, inf)
;;
;; The continued fraction, valid for all z except the negative real
;; axis:
;;
;; b[n] = 1+2*n+z-a
;; a[n] = n*(a-n)
;;
;; See http://functions.wolfram.com/06.06.10.0003.01
(defun cf-incomplete-gamma-tail (a z)
  (when (and (zerop (imagpart z)) (minusp (realpart z)))
    (error 'domain-error
	   :function-name 'cf-incomplete-gamma-tail
	   :format-arguments (list 'z z)
	   :format-control "Argument ~S should not be on the negative real axis:  ~S"))
  (/ (handler-case (* (expt z a)
		      (exp (- z)))
       (arithmetic-error ()
	 ;; z^a*exp(-z) can overflow prematurely.  In this case, use
	 ;; the equivalent exp(a*log(z)-z).  We don't use this latter
	 ;; form because it has more roundoff error than the former.
	 (exp (- (* a (log z)) z))))
     (let ((z-a (- z a)))
       (lentz #'(lambda (n)
		  (+ n n 1 z-a))
	      #'(lambda (n)
		  (* n (- a n)))))))

;; Incomplete gamma function:
;; integrate(x^(a-1)*exp(-x), x, 0, z)
;;
;; The continued fraction, valid for all z:
;;
;; b[n] = n - 1 + z + a
;; a[n] = -z*(a + n)
;;
;; See http://functions.wolfram.com/06.06.10.0007.01.  We modified the
;; continued fraction slightly and discarded the first quotient from
;; the fraction.
#+nil
(defun cf-incomplete-gamma (a z)
  (/ (handler-case (* (expt z a)
		      (exp (- z)))
       (arithmetic-error ()
	 ;; z^a*exp(-z) can overflow prematurely.  In this case, use
	 ;; the equivalent exp(a*log(z)-z).  We don't use this latter
	 ;; form because it has more roundoff error than the former.
	 (exp (- (* a (log z)) z))))
     (let ((za1 (+ z a 1)))
       (- a (/ (* a z)
	       (lentz #'(lambda (n)
			  (+ n za1))
		      #'(lambda (n)
			  (- (* z (+ a n))))))))))

;; Incomplete gamma function:
;; integrate(x^(a-1)*exp(-x), x, 0, z)
;;
;; The continued fraction, valid for all z:
;;
;; b[n] = a + n
;; a[n] = -(a+n/2)*z if n odd
;;        n/2*z      if n even
;;
;; See http://functions.wolfram.com/06.06.10.0009.01.
;;
;; Some experiments indicate that this converges faster than the above
;; and is actually quite a bit more accurate, expecially near the
;; negative real axis.
(defun cf-incomplete-gamma (a z)
  (/ (handler-case (* (expt z a)
		      (exp (- z)))
       (arithmetic-error ()
	 ;; z^a*exp(-z) can overflow prematurely.  In this case, use
	 ;; the equivalent exp(a*log(z)-z).  We don't use this latter
	 ;; form because it has more roundoff error than the former.
	 (exp (- (* a (log z)) z))))
     (lentz #'(lambda (n)
		(+ n a))
	    #'(lambda (n)
		(if (evenp n)
		    (* (ash n -1) z)
		    (- (* (+ a (ash n -1)) z)))))))

;; Series expansion for incomplete gamma.  Intended for |a|<1 and
;; |z|<1.  The series is
;;
;; g(a,z) = z^a * sum((-z)^k/k!/(a+k), k, 0, inf)
(defun s-incomplete-gamma (a z)
  (let ((-z (- z))
	(eps (epsilon z)))
    (loop for k from 0
       for term = 1 then (* term (/ -z k))
       for sum = (/ a) then (+ sum (/ term (+ a k)))
       when (< (abs term) (* (abs sum) eps))
       return (* sum (expt z a)))))

;; Tail of the incomplete gamma function.
(defun incomplete-gamma-tail (a z)
  "Tail of the incomplete gamma function defined by:

  integrate(t^(a-1)*exp(-t), t, z, inf)"
  (with-floating-point-contagion (a z)
    (if (and (realp a) (<= a 0))
	;; incomplete_gamma_tail(v, z) = z^v*exp_integral_e(1-a,z)
	(* (expt z a)
	   (exp-integral-e (- 1 a) z))
	(if (and (zerop (imagpart a))
		 (zerop (imagpart z)))
	    ;; For real values, we split the result to compute either the
	    ;; tail directly from the continued fraction or from gamma(a)
	    ;; - incomplete-gamma.  The continued fraction doesn't
	    ;; converge on the negative real axis, so we can't use that
	    ;; there.  And accuracy appears to be better if z is "small".
	    ;; We take this to mean |z| < |a-1|.  Note that |a-1| is the
	    ;; peak of the integrand.
	    (if (and (> (abs z) (abs (- a 1)))
		     (not (minusp (realpart z))))
		(cf-incomplete-gamma-tail a z)
		(- (gamma a) (cf-incomplete-gamma a z)))
	    ;; If the argument is close enough to the negative real axis,
	    ;; the continued fraction for the tail is not very accurate.
	    ;; Use the incomplete gamma function to evaluate in this
	    ;; region.  (Arbitrarily selected the region to be a sector.
	    ;; But what is the correct size of this sector?)
	    (if (<= (abs (phase z)) 3.1)
		(cf-incomplete-gamma-tail a z)
		(- (gamma a) (cf-incomplete-gamma a z)))))))

(defun incomplete-gamma (a z)
  "Incomplete gamma function defined by:

  integrate(t^(a-1)*exp(-t), t, 0, z)"
  (with-floating-point-contagion (a z)
    (if (and (< (abs a) 1) (< (abs z) 1))
	(s-incomplete-gamma a z)
	(if (and (realp a) (realp z))
	    (if (< z (- a 1))
		(cf-incomplete-gamma a z)
		(- (gamma a) (cf-incomplete-gamma-tail a z)))
	    ;; The continued fraction doesn't converge very fast if a
	    ;; and z are small.  In this case, use the series
	    ;; expansion instead, which converges quite rapidly.
	    (if (< (abs z) (abs a))
		(cf-incomplete-gamma a z)
		(- (gamma a) (cf-incomplete-gamma-tail a z)))))))

(defun erf (z)
  "Error function:

    erf(z) = 2/sqrt(%pi)*sum((-1)^k*z^(2*k+1)/k!/(2*k+1), k, 0, inf)

  For real z, this is equivalent to

    erf(z) = 2/sqrt(%pi)*integrate(exp(-t^2), t, 0, z) for real z."
  ;;
  ;; Erf is an odd function: erf(-z) = -erf(z)
  (if (minusp (realpart z))
      (- (erf (- z)))
      (/ (incomplete-gamma 1/2 (* z z))
	 (sqrt (float-pi z)))))

(defun erfc (z)
  "Complementary error function:

    erfc(z) = 1 - erf(z)"
  ;; Compute erfc(z) via 1 - erf(z) is not very accurate if erf(z) is
  ;; near 1.  Wolfram says
  ;;
  ;; erfc(z) = 1 - sqrt(z^2)/z * (1 - 1/sqrt(pi)*gamma_incomplete_tail(1/2, z^2))
  ;;
  ;; For real(z) > 0, sqrt(z^2)/z is 1 so
  ;;
  ;; erfc(z) = 1 - (1 - 1/sqrt(pi)*gamma_incomplete_tail(1/2,z^2))
  ;;         = 1/sqrt(pi)*gamma_incomplete_tail(1/2,z^2)
  ;;
  ;; For real(z) < 0, sqrt(z^2)/z is -1 so
  ;;
  ;; erfc(z) = 1 + (1 - 1/sqrt(pi)*gamma_incomplete_tail(1/2,z^2))
  ;;         = 1 + 1/sqrt(pi)*gamma_incomplete(1/2,z^2)
  (if (>= (realpart z) 0)
      (/ (incomplete-gamma-tail 1/2 (* z z))
	 (sqrt (float-pi z)))
      (+ 1
	 (/ (incomplete-gamma 1/2 (* z z))
	    (sqrt (float-pi z))))))

(defun cf-exp-integral-e (v z)
  ;; We use the continued fraction
  ;;
  ;; E(v,z) = exp(-z)/cf(z)
  ;;
  ;; where the continued fraction cf(z) is
  ;;
  ;; a[k] = -k*(k+v-1)
  ;; b[k] = v + 2*k + z
  ;;
  ;; for k = 1, inf
  (let ((z+v (+ z v)))
    (/ (exp (- z))
       (lentz #'(lambda (k)
		  (+ z+v (* 2 k)))
	      #'(lambda (k)
		  (* (- k)
		     (+ k v -1)))))))


;; For v not an integer:
;;
;; E(v,z) = gamma(1-v)*z^(v-1) - sum((-1)^k*z^k/(k-v+1)/k!, k, 0, inf)
;;
;; For v an integer:
;;
;; E(v,z) = (-z)^(v-1)/(v-1)!*(psi(v)-log(z))
;;          - sum((-1)^k*z^k/(k-v+1)/k!, k, 0, inf, k != n-1)
;;
(defun s-exp-integral-e (v z)
  ;; E(v,z) = gamma(1-v)*z^(v-1) - sum((-1)^k*z^k/(k-v+1)/k!, k, 0, inf)
  (let ((-z (- z))
	(-v (- v))
	(eps (epsilon z)))
    (if (and (realp v)
	     (= v (ftruncate v)))
	;; v is an integer
	(let* ((n (truncate v))
	       (n-1 (1- n)))
	  (- (* (/ (expt -z n-1)
		   (gamma v))
		(- (psi v) (log z)))
	     (loop for k from 0
		   for term = 1 then (* term (/ -z k))
		   for sum = (if (zerop n-1)
				 0
				 (/ (- 1 v)))
		     then (+ sum (let ((denom (- k n-1)))
				   (if (zerop denom)
				       0
				       (/ term denom))))
		   when (< (abs term) (* (abs sum) eps))
		     return sum)))
	(loop for k from 0
	      for term = 1 then (* term (/ -z k))
	      for sum = (/ (- 1 v)) then (+ sum (/ term (+ k 1 -v)))
	      when (< (abs term) (* (abs sum) eps))
		return (- (* (gamma (- 1 v)) (expt z (- v 1)))
				    sum)))))

(defun exp-integral-e (v z)
  "Exponential integral E:

   E(v,z) = integrate(exp(-t)/t^v, t, 1, inf)"
  ;; E(v,z) = z^(v-1) * integrate(t^(-v)*exp(-t), t, z, inf);
  ;;
  ;; for |arg(z)| < pi.
  ;;
  ;;
  (with-floating-point-contagion (v z)
    (cond ((and (realp v) (minusp v))
	   ;; E(-v, z) = z^(-v-1)*incomplete_gamma_tail(v+1,z)
	   (let ((-v (- v)))
	     (* (expt z (- v 1))
		(incomplete-gamma-tail (+ -v 1) z))))
	  ((or (< (abs z) 1) (>= (abs (phase z)) 3.1))
	   ;; Use series for small z or if z is near the negative real
	   ;; axis because the continued fraction does not converge on
	   ;; the negative axis and converges slowly near the negative
	   ;; axis.
	   (s-exp-integral-e v z))
	  (t
	   ;; Use continued fraction for everything else.
	   (cf-exp-integral-e v z)))))

;; Series for Fresnel S
;;
;;   S(z) = z^3*sum((%pi/2)^(2*k+1)(-z^4)^k/(2*k+1)!/(4*k+3), k, 0, inf)
;;
;; Compute as
;;
;;   S(z) = z^3*sum(a(k)/(4*k+3), k, 0, inf)
;;
;; where
;;
;;   a(k+1) = -a(k) * (%pi/2)^2 * z^4 / (2*k+2) / (2*k+3)
;;
;;   a(0) = %pi/2.
(defun fresnel-s-series (z)
  (let* ((pi/2 (* 1/2 (float-pi z)))
	 (factor (- (* (expt z 4) pi/2 pi/2)))
	 (eps (epsilon z))
	 (sum 0)
	 (term pi/2))
    (loop for k2 from 0 by 2
       until (< (abs term) (* eps (abs sum)))
       do
       (incf sum (/ term (+ 3 k2 k2)))
       (setf term (/ (* term factor)
		     (* (+ k2 2)
			(+ k2 3)))))
    (* sum (expt z 3))))
    
(defun fresnel-s (z)
  "Fresnel S:

   S(z) = integrate(sin(%pi*t^2/2), t, 0, z) "
  (let ((prec (float-contagion z))
	(sqrt-pi (sqrt (float-pi z))))
    (flet ((fs (z)
	     ;; Wolfram gives
	     ;;
	     ;;  S(z) = (1+%i)/4*(erf(c*z) - %i*erf(conjugate(c)*z))
	     ;;
	     ;; where c = sqrt(%pi)/2*(1+%i).
	     ;;
	     ;; But for large z, we should use erfc.  Then
	     ;;  S(z) = 1/2 - (1+%i)/4*(erfc(c*z) - %i*erfc(conjugate(c)*z))
	     (if (and t (> (abs z) 2))
		 (- 1/2
		    (* #c(1/4 1/4)
		       (- (erfc (* #c(1/2 1/2) sqrt-pi z))
			  (* #c(0 1)
			     (erfc (* #c(1/2 -1/2) sqrt-pi z))))))
		 (* #c(1/4 1/4)
		    (- (erf (* #c(1/2 1/2) sqrt-pi z))
		       (* #c(0 1)
			  (erf (* #c(1/2 -1/2) sqrt-pi z)))))))
          (rfs (z)
            ;; When z is real, recall that erf(conjugate(z)) =
            ;; conjugate(erf(z)).  Then
            ;;
            ;;  S(z) = 1/2*(realpart(erf(c*z)) - imagpart(erf(c*z)))
            ;;
            ;; But for large z, we should use erfc.  Then
            ;;
            ;;  S(z) = 1/2 - 1/2*(realpart(erfc(c*z)) - imagpart(erf(c*z)))
            (if (> (abs z) 2)
                (let ((s (erfc (* #c(1/2 1/2) sqrt-pi z))))
                  (- 1/2
                     (* 1/2 (- (realpart s) (imagpart s)))))
                (let ((s (erf (* #c(1/2 1/2) sqrt-pi z))))
                  (* 1/2 (- (realpart s) (imagpart s)))))))
      ;; For small z, the erf terms above suffer from subtractive
      ;; cancellation.  So use the series in this case.  Some simple
      ;; tests were done to determine that for double-floats we want
      ;; to use the series for z < 1 to give max accuracy.  For
      ;; qd-real, the above formula is good enough for z > 1d-5.
      (if (< (abs z) (ecase prec
		       (single-float 1.5f0)
		       (double-float 1d0)
		       (qd-real #q1)))
	  (fresnel-s-series z)
	  (if (realp z)
	      ;; FresnelS is real for a real argument. And it is odd.
	      (if (minusp z)
		  (- (rfs (- z)))
		  (rfs z))
	      (fs z))))))

(defun fresnel-c (z)
  "Fresnel C:

   C(z) = integrate(cos(%pi*t^2/2), t, 0, z) "
  (let ((sqrt-pi (sqrt (float-pi z))))
    (flet ((fs (z)
	     ;; Wolfram gives
	     ;;
	     ;;  C(z) = (1-%i)/4*(erf(c*z) + %i*erf(conjugate(c)*z))
	     ;;
	     ;; where c = sqrt(%pi)/2*(1+%i).
	     (* #c(1/4 -1/4)
		(+ (erf (* #c(1/2 1/2) sqrt-pi z))
		   (* #c(0 1)
		      (erf (* #c(1/2 -1/2) sqrt-pi z)))))))
      (if (realp z)
	  ;; FresnelS is real for a real argument. And it is odd.
	  (if (minusp z)
	      (- (realpart (fs (- z))))
	      (realpart (fs z)))
	  (fs z)))))

(defun sin-integral (z)
  "Sin integral:

  Si(z) = integrate(sin(t)/t, t, 0, z)"
  ;; Wolfram has
  ;;
  ;; Si(z) = %i/2*(gamma_inc_tail(0, -%i*z) - gamma_inc_tail(0, %i*z) + log(-%i*z)-log(%i*z))
  ;;
  (flet ((si (z)
	   (* #c(0 1/2)
	      (let ((iz (* #c(0 1) z))
		    (-iz (* #c(0 -1) z)))
		(+ (- (incomplete-gamma-tail 0 -iz)
		      (incomplete-gamma-tail 0 iz))
		   (- (log -iz)
		      (log iz)))))))
    (if (realp z)
	;; Si is odd and real for real z.  In this case, we have
	;;
	;; Si(x) = %i/2*(gamma_inc_tail(0, -%i*x) - gamma_inc_tail(0, %i*x) - %i*%pi)
	;;       = %pi/2 + %i/2*(gamma_inc_tail(0, -%i*x) - gamma_inc_tail(0, %i*x))
	;; But gamma_inc_tail(0, conjugate(z)) = conjugate(gamma_inc_tail(0, z)), so
	;;
	;; Si(x) = %pi/2 + imagpart(gamma_inc_tail(0, %i*x))
	(cond ((< z 0)
	       (- (sin-integral (- z))))
	      ((= z 0)
	       (* 0 z))
	      (t
	       (+ (* 1/2 (float-pi z))
		  (imagpart (incomplete-gamma-tail 0 (complex 0 z))))))
	(si z))))

(defun cos-integral (z)
  "Cos integral:

    Ci(z) = integrate((cos(t) - 1)/t, t, 0, z) + log(z) + gamma

  where gamma is Euler-Mascheroni constant"
  ;; Wolfram has
  ;;
  ;; Ci(z) = log(z) - 1/2*(gamma_inc_tail(0, -%i*z) + gamma_inc_tail(0, %i*z) + log(-%i*z)+log(%i*z))
  ;;
  (flet ((ci (z)
	   (- (log z)
	      (* 1/2
		 (let ((iz (* #c(0 1) z))
		       (-iz (* #c(0 -1) z)))
		   (+ (+ (incomplete-gamma-tail 0 -iz)
			 (incomplete-gamma-tail 0 iz))
		      (+ (log -iz)
			 (log iz))))))))
    (if (and (realp z) (plusp z))
	(realpart (ci z))
	(ci z))))

;; Array of values of the Bernoulli numbers.  We only have enough for
;; the evaluation of the psi function.
(defparameter bern-values
  (make-array 55
	      :initial-contents
	      '(1
		-1/2
		1/6
		0
		-1/30
		0
		1/42
		0
		-1/30
		0
		5/66
		0
		-691/2730
		0
		7/6
		0
		-3617/510
		0
		43867/798
		0
		-174611/330
		0
		854513/138
		0
		-236364091/2730
		0
		8553103/6
		0
		-23749461029/870
		0
		8615841276005/14322
		0
		-7709321041217/510
		0
		2577687858367/6
		0
		-26315271553053477373/1919190
		0
		2929993913841559/6
		0
		-261082718496449122051/13530
		0
		1520097643918070802691/1806
		0
		-27833269579301024235023/690
		0
		596451111593912163277961/282
		0
		-5609403368997817686249127547/46410
		0
		495057205241079648212477525/66
		0
		-801165718135489957347924991853/1590
		0
		29149963634884862421418123812691/798
		)))
		
(defun bern (k)
  (aref bern-values k))

(defun psi (z)
  "Digamma function defined by

  - %gamma + sum(1/k-1/(k+z-1), k, 1, inf)

  where %gamma is Euler's constant"

  ;; A&S 6.3.7:  Reflection formula
  ;;
  ;;   psi(1-z) = psi(z) + %pi*cot(%pi*z)
  ;;
  ;; A&S 6.3.6:  Recurrence formula
  ;;
  ;;   psi(n+z) = 1/(z+n-1)+1/(z+n-2)+...+1/(z+2)+1/(1+z)+psi(1+z)
  ;;
  ;; A&S 6.3.8:  Asymptotic formula
  ;;
  ;;   psi(z) ~ log(z) - sum(bern(2*n)/(2*n*z^(2*n)), n, 1, inf)
  ;;
  ;; So use reflection formula if Re(z) < 0.  For z > 0, use the recurrence
  ;; formula to increase the argument and then apply the asymptotic formula.

  (cond ((= z 1)
	 ;; psi(1) = -%gamma
	 (- (float +%gamma+ (if (integerp z) 0.0 z))))
	((minusp (realpart z))
	 (let ((p (float-pi z)))
	   (flet ((cot-pi (z)
		    ;; cot(%pi*z), carefully.  If z is an odd multiple
		    ;; of 1/2, cot is 0.
		    (if (and (realp z)
			     (= 1/2 (abs (- z (ftruncate z)))))
			(float 0 z)
			(/ (tan (* p z))))))
	     (- (psi (- 1 z))
		(* p (cot-pi z))))))
	(t
	 (let* ((k (* 2 (1+ (floor (* .41 (- (log (epsilon (float (realpart z))) 10)))))))
		(m 0)
		(y (expt (+ z k) 2))
		(x 0))
	   (loop for i from 1 upto (floor k 2) do
	     (progn
	       (incf m (+ (/ (+ z i i -1))
			  (/ (+ z i i -2))))
	       (setf x (/ (+ x (/ (bern (+ k 2 (* -2 i)))
				  (- k i i -2)))
			  y))))
	   (- (log (+ z k))
	      (/ (* 2 (+ z k)))
	      x
	      m)))))

