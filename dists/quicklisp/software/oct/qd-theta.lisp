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

;; Theta functions
;;
;; theta[1](z,q) = 2*sum((-1)^n*q^((n+1/2)^2)*sin((2*n+1)*z), n, 0, inf)
;;
;; theta[2](z,q) = 2*sum(q^((n+1/2)^2)*cos((2*n+1)*z), n, 0, inf)
;;
;; theta[3](z,q) = 1+2*sum(q^(n*n)*cos(2*n*z), n, 1, inf)
;;
;; theta[4](z,q) = 1+2*sum((-1)^n*q^(n*n)*cos(2*n*z), n, 1, inf)
;;
;; where q is the nome, related to parameter tau by q =
;; exp(%i*%pi*tau), or %pi*tau = log(q)/%i.
;;
;; In all cases |q| < 1.


;; The algorithms for computing the theta functions were given to me
;; by Richard Gosper (yes, that Richard Gosper).  These came from
;; package for maxima for the theta functions.

;; e1 M[1,3] + e2 M[2,3] + e3, where M = prod(mat(a11 ... a23 0 0 1))
;; where fun(k,matfn) supplies the upper six a[ij](k) to matfn.
;;
;; This is clearer if you look at the formulas below for the theta functions.
(defun 3by3rec (e1 e2 e3 fun)
  (do ((k 0 (+ k 1)))
      ((= e3 (funcall fun k
		      #'(lambda (a11 a12 a13 a21 a22 a23) ;&opt (a31 0) (a32 0) (a33 1)
			  (psetf e1 (+ (* a11 e1) (* a21 e2))
				 e2 (+ (* a12 e1) (* a22 e2))
				 e3 (+ (* a13 e1) (* a23 e2) e3))
			  (+ e3 (abs e1) (abs e2)))))
       e3)))

;;                     inf  [      2 n                 1/4 ]
;;                    /===\ [ - 2 q    cos(2 z)  1  2 q    ]
;;                     | |  [                              ]
;;[sin(z), sin(z), 0]  | |  [       4 n - 2                ] = [0, 0, theta (z, q)]
;;                     | |  [    - q             0    0    ]               1
;;                    n = 1 [                              ]
;;                          [         0          0    1    ]

(defun elliptic-theta-1 (z q)
  "Elliptic theta function 1

   theta1(z, q) = 2*q^(1/4)*sum((-1)^n*q^(n*(n+1))*sin((2*n+1)*z), n, 0, inf)"
  (let* ((precision (float-contagion z q))
	 (z (apply-contagion z precision))
	 (q (apply-contagion q precision))
	 (s (sin z))
	 (q^2 (* q q))
	 (q^4 (* q^2 q^2))
	 (-q^4n-2 (/ -1 q^2))
	 (-2q^2ncos (* -2 (cos (* 2 z))))
	 (2q^1/4 (* 2 (sqrt (sqrt q)))))
    (3by3rec s s 0
	     #'(lambda (k matfun)
		 (declare (ignore k))
		 (funcall matfun
			  (setf -2q^2ncos (* q^2 -2q^2ncos))
			  1
			  2q^1/4
			  (setf -q^4n-2 (* q^4 -q^4n-2))
			  0
			  0)))))

;;                    inf  [    2 k + 1                ]
;;                   /===\ [ 2 q        cos(2 z)  1  2 ]
;;                    | |  [                           ]
;;[q cos(2 z), 1, 1]  | |  [          4 k              ] = [0, 0, theta (z)]
;;                    | |  [       - q            0  0 ]               3
;;                   k = 1 [                           ]
;;                         [          0           0  1 ]
(defun elliptic-theta-3 (z q)
  "Elliptic theta function 3

  theta3(z, q) = 1 + 2 * sum(q^(n^2)*cos(2*n*z), n, 1, inf)"
  (let* ((precision (float-contagion z q))
	 (z (apply-contagion z precision))
	 (q (apply-contagion q precision))
	 (q^2 (* q q))
	 (q^2k 1.0)
	 (cos (cos (* 2 z))))
    (3by3rec (* q cos) 1 1
	     #'(lambda (k matfun)
		 (declare (ignore k))
		 (funcall matfun
			  (* 2 (* (setf q^2k (* q^2 q^2k)) q cos))
			  1
			  2
			  (- (* q^2k q^2k))
			  0
			  0)))))

;; theta[2](z,q) = theta[1](z+%pi/2, q)
(defun elliptic-theta-2 (z q)
  "Elliptic theta function 2

  theta2(z, q) = 2*q^(1/4)*sum(q^(n*(n+1))*cos((2*n+1)*z), n, 0, inf)"
  (let* ((precision (float-contagion z q))
	 (z (apply-contagion z precision))
	 (q (apply-contagion q precision)))
    (elliptic-theta-1 (+ z (/ (float-pi z) 2)) q)))

;; theta[4](z,q) = theta[3](z+%pi/2,q)
(defun elliptic-theta-4 (z q)
  "Elliptic theta function 4

  theta4(z, q) = 1 + 2*sum((-1)^n*q^(n^2)*cos(2*n*z), n, 1, inf)"
  (let* ((precision (float-contagion z q))
	 (z (apply-contagion z precision))
	 (q (apply-contagion q precision)))
    (elliptic-theta-3 (+ z (/ (float-pi z) 2)) q)))

(defun elliptic-theta (n z q)
  "Elliptic Theta function n where n = 1, 2, 3, or 4."
  (ecase n
    (1 (elliptic-theta-1 z q))
    (2 (elliptic-theta-2 z q))
    (3 (elliptic-theta-3 z q))
    (4 (elliptic-theta-4 z q))))
    
;; The nome, q, is given by q = exp(-%pi*K'/K) where K and %i*K' are
;; the quarter periods.
(defun elliptic-nome (m)
  "Compute the elliptic nome, q, from the parameter m"
  (exp (- (/ (* (float-pi m) (elliptic-k (- 1 m)))
	     (elliptic-k m)))))

