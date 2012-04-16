#+xcvb (module (:depends-on ("pkgdcl")))

;;; Load-time Initialization.

(in-package :asdf-encodings)

(initialize-normalized-encodings)
(register-asdf-encodings)
