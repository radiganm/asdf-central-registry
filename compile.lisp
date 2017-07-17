#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
;; compile.lisp
;; Copyright 2016 Mac Radigan
;; All Rights Reserved

  (setf sb-impl::*default-external-format* :UTF-8)

  (require 'asdf)
  (setf asdf:*central-registry*
    (list* '*default-pathname-defaults* 
     #p"/opt/asdf-central-registry/dists/quicklisp/software/oct/"
     asdf:*central-registry*))
  (probe-file "quicklisp.asd")
  (asdf:oos 'asdf:load-op :oct)

;; *EOF*
