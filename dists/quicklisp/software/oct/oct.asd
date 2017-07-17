;;;; -*- Mode: lisp -*-
;;;;
;;;; Copyright (c) 2007, 2011 Raymond Toy
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

;;; This is the asdf definition for oct.  I don't normally use this,
;;; so it might be out of date.  Use at your own risk.

(defpackage #:oct-system
  (:use #:cl #:asdf))

(in-package #:oct-system)

(asdf:defsystem oct
  :description "A portable implementation of quad-double arithmetic.  See <http://www.common-lisp.net/project/oct>."
  :author "Raymond Toy"
  :maintainer "See <http://www.common-lisp.net/project/oct>"
  :licence "MIT"
  :version "2013.11.26"			; Just use the date
  :components
  ((:file "qd-package")
   (:file "qd-rep" :depends-on ("qd-package"))
   #-cmu
   (:file "qd-dd" :depends-on ("qd-package" "qd-rep"))
   (:file "qd"
	  :depends-on ("qd-rep" #-cmu "qd-dd"))
   (:file "qd-io"
	  :depends-on ("qd"))
   (:file "qd-const"
	  :depends-on ("qd-io")
	  :around-compile (lambda (thunk)
			    ;; Just byte-compile these on cmucl since these are just constants
			    (let (#+nil (ext:*byte-compile-default* t))
			      (funcall thunk))))
   (:file "qd-fun"
	  :depends-on ("qd" "qd-const"))
   (:file "qd-class"
	  :depends-on ("qd-fun"))
   (:file "qd-const2" :depends-on ("qd-class" "qd-const")
	  :around-compile (lambda (thunk)
			    ;; Just byte-compile these on cmucl since these are just constants
			    (let (#+nil (ext:*byte-compile-default* t))
			      (funcall thunk))))
   (:file "qd-methods"
	  :depends-on ("qd-class"))
   (:file "qd-reader"
	  :depends-on ("qd-methods"))
   (:file "qd-format"
	  :depends-on ("qd-methods" "qd-reader"))
   (:file "qd-complex"
	  :depends-on ("qd-methods" "qd-reader"))
   (:file "qd-elliptic"
	  :depends-on ("qd-methods" "qd-reader"))
   (:file "qd-theta"
	  :depends-on ("qd-methods" "qd-reader"))
   (:file "qd-gamma"
	  :depends-on ("qd-complex" "qd-methods" "qd-reader"))
   (:file "qd-bessel"
	  :depends-on ("qd-methods"))))


(defmethod perform ((op test-op) (c (eql (asdf:find-system :oct))))
  (oos 'test-op 'oct-tests))

(asdf:defsystem oct-tests
  :depends-on (oct)
  :version "2013.11.26"			; Just use the date
  :in-order-to ((compile-op (load-op :rt))
		(load-op (load-op :rt))
		(test-op (load-op :rt :oct)))
  :components
  ((:file "qd-extra")
   (:file "qd-test")
   (:file "rt-tests")))

(defmethod perform ((op test-op) (c (eql (asdf:find-system :oct-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RT")))
      (error "TEST-OP failed for OCT-TESTS")))
