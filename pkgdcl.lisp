#+xcvb (module ())

(in-package :cl)

(defpackage :asdf-encodings
  (:use :cl)
  (:export
   #:encoding-external-format
   #:*on-unsupported-encoding*
   #:detect-file-encoding
   #:normalize-encoding
   #:find-implementation-encoding))
