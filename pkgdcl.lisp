#+xcvb (module ())

(in-package :cl)

(defpackage :asdf-encodings
  (:use :cl)
  (:export
   #:encoding-external-format
   #:normalize-encoding
   #:find-implementation-encoding
   #:*on-unsupported-encoding*))
