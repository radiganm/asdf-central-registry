#!/bin/sh
#|
exec sbcl --script $0 $0 ${1+"$@"}
exit
|#
;; bootstrap-quicklisp.lisp
;; Mac Radigan

  (sb-ext:run-program "/usr/bin/env" '("curl" "-O" "https://beta.quicklisp.org/quicklisp.lisp"))
  (load "./quicklisp.lisp")
  (quicklisp-quickstart:install)
  (load "~/quicklisp/setup.lisp")
  (ql:quickload "cl-template")
  (ql:quickload "alexandria")
  (ql:quickload "bordeaux-threads")
  (ql:quickload "base")
  (ql:quickload "quicklisp-slime-helper")
  (ql:quickload "split-sequence")
  (ql:quickload "swank-client")
  (ql:quickload "unix-options")
  (ql:quickload "usocket")

;; *EOF*
