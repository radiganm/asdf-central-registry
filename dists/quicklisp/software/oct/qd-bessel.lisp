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

;;; References:
;;;
;;; [1] Borwein, Borwein, Crandall, "Effective Laguerre Asymptotics",
;;; http://people.reed.edu/~crandall/papers/Laguerre-f.pdf
;;;
;;; [2] Borwein, Borwein, Chan, "The Evaluation of Bessel Functions
;;; via Exp-Arc Integrals", http://web.cs.dal.ca/~jborwein/bessel.pdf
;;;

(defvar *debug-exparc* nil)

;; B[k](p) = 1/2^(k+3/2)*integrate(exp(-p*u)*u^(k-1/2),u,0,1)
;;         = 1/2^(k+3/2)/p^(k+1/2)*integrate(t^(k-1/2)*exp(-t),t,0,p)
;;         = 1/2^(k+3/2)/p^(k+1/2) * G(k+1/2, p)
;;
;; where G(a,z) is the lower incomplete gamma function.
;;
;; There is the continued fraction expansion for G(a,z) (see
;; cf-incomplete-gamma in qd-gamma.lisp):
;;
;;  G(a,z) = z^a*exp(-z)/ CF
;;
;; So
;;
;;  B[k](p) = 1/2^(k+3/2)/p^(k+1/2)*p^(k+1/2)*exp(-p)/CF
;;          = exp(-p)/2^(k+3/2)/CF
;;
;;
;; Note also that [2] gives a recurrence relationship for B[k](p) in
;; eq (2.6), but there is an error there.  The correct relationship is
;;
;;  B[k](p) = -exp(-p)/(p*sqrt(2)*2^(k+1)) + (k-1/2)*B[k-1](p)/(2*p)
;;
;; The paper is missing the division by p in the term containing
;; B[k-1](p).  This is easily derived from the recurrence relationship
;; for the (lower) incomplete gamma function.
;;
;; Note too that as k increases, the recurrence appears to be unstable
;; and B[k](p) begins to increase even though it is strictly bounded.
;; (This is also easy to see from the integral.)  Hence, we do not use
;; the recursion.  However, it might be stable for use with
;; double-float precision; this has not been tested.
;;
(defun bk (k p)
  (/ (exp (- p))
     (* (sqrt (float 2 (realpart p))) (ash 1 (+ k 1)))
     (let ((a (float (+ k 1/2) (realpart p))))
       (lentz #'(lambda (n)
		  (+ n a))
	      #'(lambda (n)
		  (if (evenp n)
		      (* (ash n -1) p)
		      (- (* (+ a (ash n -1)) p))))))))

;; Use the recursion
(defun bk-iter (k p old-bk)
  (with-floating-point-contagion (p old-bk)
    (if (zerop k)
	(* (sqrt (/ (float-pi p) 8))
	   (let ((rp (sqrt p)))
	     (/ (erf rp)
		rp)))
	(- (* (- k 1/2)
	      (/ old-bk (* 2 p)))
	   (/ (exp (- p))
	      p
	      (ash 1 (+ k 1))
	      (sqrt  (float 2 (realpart p))))))))

;; exp-arc I function, as given in the Laguerre paper
;;
;; I(p, q) = 4*exp(p) * sum(g[k](-2*%i*q)/(2*k)!*B[k](p), k, 0, inf)
;;
;; where g[k](p) = product(p^2+(2*j-1)^2, j, 1, k) and B[k](p) as above.
;;
;; For computation, note that g[k](p) = g[k-1](p) * (p^2 + (2*k-1)^2)
;; and (2*k)! = (2*k-2)! * (2*k-1) * (2*k).  Then, let
;;
;;  R[k](p) = g[k](p)/(2*k)!
;;
;; Then
;;
;;  R[k](p) = g[k](p)/(2*k)!
;;          = g[k-1](p)/(2*k-2)! * (p^2 + (2*k-1)^2)/((2*k-1)*(2*k)
;;          = R[k-1](p) * (p^2 + (2*k-1)^2)/((2*k-1)*(2*k)
;;
;; In the exp-arc paper, the function is defined (equivalently) as
;; 
;; I(p, q) = 2*%i*exp(p)/q * sum(r[2*k+1](-2*%i*q)/(2*k)!*B[k](p), k, 0, inf)
;;
;; where r[2*k+1](p) = p*product(p^2 + (2*j-1)^2, j, 1, k)
;;
;; Let's note some properties of I(p, q).
;;
;; I(-%i*z, v) = 2*%i*exp(-%i*z)/q * sum(r[2*k+1](-2*%i*v)/(2*k)!*B[k](-%i*z))
;;
;; Note thate B[k](-%i*z) = 1/2^(k+3/2)*integrate(exp(%i*z*u)*u^(k-1/2),u,0,1)
;;                        = conj(B[k](%i*z).
;;
;; Hence I(-%i*z, v) = conj(I(%i*z, v)) when both z and v are real.
;;
;; Also note that when v is an integer of the form (2*m+1)/2, then
;;   r[2*k+1](-2*%i*v) = r[2*k+1](-%i*(2*m+1))
;;                     = -%i*(2*m+1)*product(-(2*m+1)^2+(2*j-1)^2, j, 1, k)
;; so the product is zero when k >= m and the series I(p, q) is
;; finite.
(defun exp-arc-i (p q)
  (let* ((sqrt2 (sqrt (float 2 (realpart p))))
	 (exp/p/sqrt2 (/ (exp (- p)) p sqrt2))
	 (v (* #c(0 -2) q))
	 (v2 (expt v 2))
	 (eps (epsilon (realpart p))))
    (when *debug-exparc*
      (format t "sqrt2 = ~S~%" sqrt2)
      (format t "exp/p/sqrt2 = ~S~%" exp/p/sqrt2))
    (do* ((k 0 (1+ k))
	  (bk (/ (incomplete-gamma 1/2 p)
		 2 sqrt2 (sqrt p))
	      (- (/ (* bk (- k 1/2)) 2 p)
		 (/ exp/p/sqrt2 (ash 1 (+ k 1)))))
	  ;; ratio[k] = r[2*k+1](v)/(2*k)!.
	  ;; r[1] = v and r[2*k+1](v) = r[2*k-1](v)*(v^2 + (2*k-1)^2)
	  ;; ratio[0] = v
	  ;; and ratio[k] = r[2*k-1](v)*(v^2+(2*k-1)^2) / ((2*k-2)! * (2*k-1) * 2*k)
	  ;;              = ratio[k]*(v^2+(2*k-1)^2)/((2*k-1) * 2 * k)
	  (ratio v
		 (* ratio (/ (+ v2 (expt (1- (* 2 k)) 2))
			     (* 2 k (1- (* 2 k))))))
	  (term (* ratio bk)
		(* ratio bk))
	  (sum term (+ sum term)))
	 ((< (abs term) (* (abs sum) eps))
	  (* sum #c(0 2) (/ (exp p) q)))
      (when *debug-exparc*
	(format t "k      = ~D~%" k)
	(format t " bk    = ~S~%" bk)
	(format t " ratio = ~S~%" ratio)
	(format t " term  = ~S~%" term)
	(format t " sum   - ~S~%" sum)))))

(defun exp-arc-i-2 (p q)
  (let* ((v (* #c(0 -2) q))
	 (v2 (expt v 2))
	 (eps (epsilon (realpart p))))
    (do* ((k 0 (1+ k))
	  (bk (bk 0 p)
	      (bk k p))
	  ;; Compute g[k](p)/(2*k)!, not r[2*k+1](p)/(2*k)!
	  (ratio 1
		 (* ratio (/ (+ v2 (expt (1- (* 2 k)) 2))
			     (* 2 k (1- (* 2 k))))))
	  (term (* ratio bk)
		(* ratio bk))
	  (sum term (+ sum term)))
	 ((< (abs term) (* (abs sum) eps))
	  (when *debug-exparc*
	    (format t "Final k= ~D~%" k)
	    (format t " bk    = ~S~%" bk)
	    (format t " ratio = ~S~%" ratio)
	    (format t " term  = ~S~%" term)
	    (format t " sum   - ~S~%" sum))
	  (* sum 4 (exp p)))
      (when *debug-exparc*
	(format t "k      = ~D~%" k)
	(format t " bk    = ~S~%" bk)
	(format t " ratio = ~S~%" ratio)
	(format t " term  = ~S~%" term)
	(format t " sum   - ~S~%" sum)))))

(defun exp-arc-i-3 (p q)
  (let* ((v (* #c(0 -2) q))
	 (v2 (expt v 2))
	 (eps (epsilon (realpart p))))
    (do* ((k 0 (1+ k))
	  (bk (bk 0 p)
	      (bk-iter k p bk))
	  ;; Compute g[k](p)/(2*k)!, not r[2*k+1](p)/(2*k)!
	  (ratio 1
		 (* ratio (/ (+ v2 (expt (1- (* 2 k)) 2))
			     (* 2 k (1- (* 2 k))))))
	  (term (* ratio bk)
		(* ratio bk))
	  (sum term (+ sum term)))
	 ((< (abs term) (* (abs sum) eps))
	  (when *debug-exparc*
	    (format t "Final k= ~D~%" k)
	    (format t " bk    = ~S~%" bk)
	    (format t " ratio = ~S~%" ratio)
	    (format t " term  = ~S~%" term)
	    (format t " sum   - ~S~%" sum))
	  (* sum 4 (exp p)))
      (when *debug-exparc*
	(format t "k      = ~D~%" k)
	(format t " bk    = ~S~%" bk)
	(format t " ratio = ~S~%" ratio)
	(format t " term  = ~S~%" term)
	(format t " sum   - ~S~%" sum)))))


;; Not really just for Bessel J for integer orders, but in that case,
;; this is all that's needed to compute Bessel J.  For other values,
;; this is just part of the computation needed.
;;
;; Compute
;;
;;  1/(2*%pi) * (exp(-%i*v*%pi/2) * I(%i*z, v) + exp(%i*v*%pi/2) * I(-%i*z, v))
(defun integer-bessel-j-exp-arc (v z)
  (let* ((iz (* #c(0 1) z))
	 (i+ (exp-arc-i-2 iz v)))
    (cond ((and (= v (ftruncate v)) (realp z))
	   ;; We can simplify the result
	   (let ((c (exp (* v (float-pi i+) #c(0 -1/2)))))
	     (/ (+ (* c i+)
		   (* (conjugate c) (conjugate i+)))
		(float-pi i+)
		2)))
	  (t
	   (let ((i- (exp-arc-i-2 (- iz ) v)))
	     (/ (+ (* (exp (* v (float-pi i+) #c(0 -1/2)))
		      i+)
		   (* (exp (* v (float-pi i+) #c(0 1/2)))
		      i-))
		(float-pi i+)
		2))))))

;; alpha[n](z) = integrate(exp(-z*s)*s^n, s, 0, 1/2)
;; beta[n](z)  = integrate(exp(-z*s)*s^n, s, -1/2, 1/2)
;;
;; The recurrence in [2] is
;;
;; alpha[n](z) = - exp(-z/2)/2^n/z + n/z*alpha[n-1](z)
;; beta[n]z)   = ((-1)^n*exp(z/2)-exp(-z/2))/2^n/z + n/z*beta[n-1](z)
;;             = (-1)^n/(2^n)*2*sinh(z/2)/z + n/z*beta[n-1](z)
;;
;; We also note that
;;
;; alpha[n](z) = G(n+1,z/2)/z^(n+1)
;; beta[n](z)  = G(n+1,z/2)/z^(n+1) - G(n+1,-z/2)/z^(n+1)

(defun alpha (n z)
  (let ((n (float n (realpart z))))
    (/ (incomplete-gamma (1+ n) (/ z 2))
       (expt z (1+ n)))))

(defun alpha-iter (n z alpha-old)
  (if (zerop n)
      ;; (1- exp(-z/2))/z.
      (/ (- 1 (exp (* z -1/2)))
	 z)
      (- (* (/ n z) alpha-old)
	 (/ (exp (- (* z 1/2)))
	    z
	    (ash 1 n)))))

(defun beta (n z)
  (let ((n (float n (realpart z))))
    (/ (- (incomplete-gamma (1+ n) (/ z 2))
	  (incomplete-gamma (1+ n) (/ z -2)))
       (expt z (1+ n)))))

(defun beta-iter (n z old-beta)
  (if (zerop n)
      ;; integrate(exp(-z*s),s,-1/2,1/2)
      ;;   = (exp(z/2)-exp(-z/2)/z
      ;;   = 2*sinh(z/2)/z
      ;;   = sinh(z/2)/(z/2)
      (* 2 (/ (sinh (* 1/2 z)) z))
      (+ (* n (/ old-beta z))
	 (* (/ (sinh (* 1/2 z)) (* 1/2 z))
	    (scale-float (float (if (evenp n) 1 -1) (realpart z)) (- n))))))


;; a[0](k,v) := (k+sqrt(k^2+1))^(-v);
;; a[1](k,v) := -v*a[0](k,v)/sqrt(k^2+1);
;; a[n](k,v) := 1/(k^2+1)/(n-1)/n*((v^2-(n-2)^2)*a[n-2](k,v)-k*(n-1)*(2*n-3)*a[n-1](k,v));

;; Convert this to iteration instead of using this quick-and-dirty
;; memoization?
(let ((hash (make-hash-table :test 'equal)))
  (defun an-clrhash ()
    (clrhash hash))
  (defun an-dump-hash ()
    (maphash #'(lambda (k v)
		 (format t "~S -> ~S~%" k v))
	     hash))
  (defun an (n k v)
    (or (gethash (list n k v) hash)
	(let ((result
		(cond ((= n 0)
		       (expt (+ k (sqrt (float (1+ (* k k)) (realpart v)))) (- v)))
		      ((= n 1)
		       (- (/ (* v (an 0 k v))
			     (sqrt (float (1+ (* k k)) (realpart v))))))
		      (t
		       (/ (- (* (- (* v v) (expt (- n 2) 2)) (an (- n 2) k v))
			     (* k (- n 1) (+ n n -3) (an (- n 1) k v)))
			  (+ 1 (* k k))
			  (- n 1)
			  n)))))
	  (setf (gethash (list n k v) hash) result)
	  result))))

;; SUM-AN computes the series
;;
;; sum(exp(-k*z)*a[n](k,v), k, 1, N)
;;
#+nil
(defun sum-an (big-n n v z)
  (let ((sum 0))
    (loop for k from 1 upto big-n
	  do
	     (incf sum (* (exp (- (* k z)))
			  (an n k v))))
    sum))

;; Like above, but we just stop when the terms no longer contribute to
;; the sum.
(defun sum-an (big-n n v z)
  (let ((eps (epsilon (realpart z))))
    (do* ((k 1 (+ 1 k))
	  (term (* (exp (- (* k z)))
		   (an n k v))
		(* (exp (- (* k z)))
		   (an n k v)))
	  (sum term (+ sum term)))
	 ((or (<= (abs term) (* eps (abs sum)))
	      (>= k big-n))
	  sum))))

;; SUM-AB computes the series
;;
;; sum(alpha[n](z)*a[n](0,v) + beta[n](z)*sum_an(N, n, v, z), n, 0, inf)
(defun sum-ab (big-n v z)
  (let ((eps (epsilon (realpart z))))
    (an-clrhash)
    (do* ((n 0 (+ 1 n))
	  (term (+ (* (alpha n z) (an n 0 v))
		   (* (beta n z) (sum-an big-n n v z)))
		(+ (* (alpha n z) (an n 0 v))
		   (* (beta n z) (sum-an big-n n v z))))
	  (sum term (+ sum term)))
	 ((<= (abs term) (* eps (abs sum)))
	  sum)
      (when nil
	(format t "n = ~D~%" n)
	(format t " term = ~S~%" term)
	(format t " sum  = ~S~%" sum)))))

(defun sum-ab-2 (big-n v z)
  (let ((eps (epsilon (realpart z))))
    (an-clrhash)
    (do* ((n 0 (+ 1 n))
	  (alphan (alpha-iter 0 z 0)
		  (alpha-iter n z alphan))
	  (betan (beta-iter 0 z 0)
		 (beta-iter n z betan))
	  (term (+ (* alphan (an n 0 v))
		   (* betan (sum-an big-n n v z)))
		(+ (* alphan (an n 0 v))
		   (* betan (sum-an big-n n v z))))
	  (sum term (+ sum term)))
	 ((<= (abs term) (* eps (abs sum)))
	  sum)
      (when nil
	(format t "n = ~D~%" n)
	(format t " term = ~S~%" term)
	(format t " sum  = ~S~%" sum)))))

;; Convert to iteration instead of this quick-and-dirty memoization?
(let ((hash (make-hash-table :test 'equal)))
  (defun %big-a-clrhash ()
    (clrhash hash))
  (defun %big-a-dump-hash ()
    (maphash #'(lambda (k v)
		 (format t "~S -> ~S~%" k v))
	     hash))
  (defun %big-a (n v)
    (or (gethash (list n v) hash)
	(let ((result
		(cond ((zerop n)
		       (expt 2 (- v)))
		      (t
		       (* (%big-a (- n 1) v)
			  (/ (* (+ v n n -2) (+ v n n -1))
			     (* 4 n (+ n v))))))))
	  (setf (gethash (list n v) hash) result)
	  result))))

;; Computes A[n](v) =
;; (-1)^n*v*2^(-v)*pochhammer(v+n+1,n-1)/(2^(2*n)*n!)  If v is a
;; negative integer -m, use A[n](-m) = (-1)^(m+1)*A[n-m](m) for n >=
;; m.
(defun big-a (n v)
  (let ((m (ftruncate v)))
    (cond ((and (= m v) (minusp m))
	   (if (< n m)
	       (%big-a n v)
	       (let ((result (%big-a (+ n m) (- v))))
		 (if (oddp (truncate m))
		     result
		     (- result)))))
	  (t
	   (%big-a n v)))))

;; I[n](t, z, v) = exp(-t*z)/t^(2*n+v-1) *
;;                  integrate(exp(-t*z*s)*(1+s)^(-2*n-v), s, 0, inf)
;;
;; Use the substitution u=1+s to get a new integral
;;
;;   integrate(exp(-t*z*s)*(1+s)^(-2*n-v), s, 0, inf)
;;     = exp(t*z) * integrate(u^(-v-2*n)*exp(-t*u*z), u, 1, inf)
;;     = exp(t*z)*t^(v+2*n-1)*z^(v+2*n-1)*incomplete_gamma_tail(1-v-2*n,t*z)
;;
;; Thus,
;;
;;   I[n](t, z, v) = z^(v+2*n-1)*incomplete_gamma_tail(1-v-2*n,t*z)
;;
(defun big-i (n theta z v)
  (let* ((a (- 1 v n n)))
    (* (expt z (- a))
       (incomplete-gamma-tail a (* theta z)))))

(defun sum-big-ia (big-n v z)
  (let ((big-n-1/2 (+ big-n 1/2))
	(eps (epsilon z)))
    (do* ((n 0 (1+ n))
	  (term (* (big-a 0 v)
		   (big-i 0 big-n-1/2 z v))
		(* (big-a n v)
		   (big-i n big-n-1/2 z v)))
	  (sum term (+ sum term)))
	 ((<= (abs term) (* eps (abs sum)))
	  sum)
      #+nil
      (progn
	(format t "n = ~D~%" n)
	(format t " term = ~S~%" term)
	(format t " sum  = ~S~%" sum)))))

;; Series for bessel J:
;;
;; (z/2)^v*sum((-1)^k/Gamma(k+v+1)/k!*(z^2//4)^k, k, 0, inf)
(defun s-bessel-j (v z)
  (with-floating-point-contagion (v z)
    (let ((z2/4 (* z z 1/4))
	  (eps (epsilon z)))
      (do* ((k 0 (+ 1 k))
	    (f (gamma (+ v 1))
	       (* k (+ v k)))
	    (term (/ f)
		  (/ (* (- term) z2/4) f))
	    (sum term (+ sum term)))
	   ((<= (abs term) (* eps (abs sum)))
	    (* sum (expt (* z 1/2) v)))
	#+nil
	(progn
	  (format t "k = ~D~%" k)
	  (format t " f    = ~S~%" f)
	  (format t " term = ~S~%" term)
	  (format t " sum  = ~S~%" sum))))))

;; 
;; TODO:
;;  o For |z| <= 1 use the series.
;;  o Currently accuracy is not good for large z and half-integer
;;    order.
;;  o For real v and z, return a real number instead of complex.
;;  o Handle the case of Re(z) < 0. (The formulas are for Re(z) > 0:
;;    bessel_j(v,z*exp(m*%pi*%i)) = exp(m*v*%pi*%i)*bessel_j(v, z)
;;  o The paper suggests using
;;      bessel_i(v,z) = exp(-v*%pi*%i/2)*bessel_j(v, %i*z)
;;    when Im(z) >> Re(z)
;; 
(defvar *big-n* 100)
(defun bessel-j (v z)
  (let ((vv (ftruncate v)))
    ;; Clear the caches for now.
    (an-clrhash)
    (%big-a-clrhash)
    (cond ((and (= vv v) (realp z))
	   ;; v is an integer and z is real
	   (integer-bessel-j-exp-arc v z))
	  (t
	   ;; Need to fine-tune the value of big-n.
	   (let ((big-n *big-n*)
		 (vpi (* v (float-pi (realpart z)))))
	     (+ (integer-bessel-j-exp-arc v z)
		(if (= vv v)
		    0
		    (* z
		       (/ (sin vpi) vpi)
		       (+ (/ -1 z)
			  (sum-ab big-n v z)
			  (sum-big-ia big-n v z))))))))))

;; Bessel Y
;;
;; bessel_y(v, z) = 1/(2*%pi*%i)*(exp(-%i*v*%pi/2)*I(%i*v,z) - exp(%i*v*%pi/2)*I(-%i*z, v))
;;                   + z/v/%pi*((1-cos(v*%pi)/z) + S(N,z,v)*cos(v*%pi)-S(N,z,-v))
;;
;; where
;;
;;   S(N,z,v) = sum(alpha[n](z)*a[n](0,v) + beta[n](z)*sum(exp(-k*z)*a[n](k,v),k,1,N),n,0,inf)
;;               + sum(A[n](v)*I[n](N+1/2,z,v),n,0,inf)
;;
(defun bessel-y (v z)
  (flet ((ipart (v z)
	   (let* ((iz (* #c(0 1) z))
		  (c+ (exp (* v (float-pi z) 1/2)))
		  (c- (exp (* v (float-pi z) -1/2)))
		  (i+ (exp-arc-i-2 iz v))
		  (i- (exp-arc-i-2 (- iz) v)))
	     (/ (- (* c- i+) (* c+ i-))
		(* #c(0 2) (float-pi z)))))
	 (s (big-n z v)
	   (+ (sum-ab big-n v z)
	      (sum-big-ia big-n v z))))
    (let* ((big-n 100)
	   (vpi (* v (float-pi z)))
	   (c (cos vpi)))
      (+ (ipart v z)
	 (* (/ z vpi)
	    (+ (/ (- 1 c)
		  z)
	       (* c
		  (s big-n z v))
	       (- (s big-n z (- v)))))))))
	   
  

(defun paris-series (v z n)
  (labels ((pochhammer (a k)
	     (/ (gamma (+ a k))
		(gamma a)))
	   (a (v k)
	     (* (/ (pochhammer (+ 1/2 v) k)
		   (gamma (float (1+ k) z)))
		(pochhammer (- 1/2 v) k))))
    (* (loop for k from 0 below n
	     sum (* (/ (a v k)
		       (expt (* 2 z) k))
		    (/ (cf-incomplete-gamma (+ k v 1/2) (* 2 z))
		       (gamma (+ k v 1/2)))))
       (/ (exp z)
	  (sqrt (* 2 (float-pi z) z))))))

