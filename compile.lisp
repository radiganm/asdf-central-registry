#|
exec sbcl --noinform --dynamic-space-size 3000 --load $0 --end-toplevel-options "$@"
|#
;; compile.lisp
;; Copyright 2016 Mac Radigan
;; All Rights Reserved

  (setf *read-default-float-format* 'double-float)
 ;(setf sb-impl::*default-external-format* :UTF-8)
  (gc :full t)

  (require 'asdf)
  (setf asdf:*central-registry*
    (list* '*default-pathname-defaults* 
     #p"/opt/asdf-central-registry/dists/quicklisp/software/oct/"
     asdf:*central-registry*))
  (probe-file "quicklisp.asd")
 ;(mk:oos "oct" :compile)
  (asdf:oos 'asdf:load-op :oct)

;; *EOF*
