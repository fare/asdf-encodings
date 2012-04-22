#+xcvb (module (:depends-on ("encodings" "asdf-support" "autodetection")))

;;; Load-time Initialization.

(in-package :asdf-encodings)

(initialize-normalized-encodings)
(register-asdf-encodings)
