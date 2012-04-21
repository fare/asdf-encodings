#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-encodings)

(defun encoding-external-format (encoding &key (on-error *on-unsupported-encoding*))
  (or (and (eq encoding :default) :default)
      (find-implementation-encoding (or (normalize-encoding encoding) encoding))
      (ecase on-error
        ((:error) (cerror "continue using :default" "unsupported encoding ~S" encoding) nil)
        ((:warn) (warn "unsupported encoding ~S, falling back to using :default " encoding) nil)
        ((nil) nil))
      :default))

(defun register-asdf-encodings ()
  (setf asdf:*encoding-external-format-hook* 'encoding-external-format)
  (values))

