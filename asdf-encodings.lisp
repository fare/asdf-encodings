(in-package :cl-user)

(defpackage :asdf-encodings
  (:use :cl)
  (:export
   #:encoding-external-format
   #:*on-unsupported-encoding*))

(in-package :asdf-encodings)

(defvar *on-unsupported-encoding* :error
  "One of :error, :warn or nil, specifies what to do when passed an unsupported encoding.")

(defun encoding-external-format (encoding)
  (or
   (case encoding
     ((:default)
      :default)
     ((:utf-8 :utf8)
      #+(and asdf-unicode (not clisp)) :utf-8
      #+(and asdf-unicode clisp) charset:utf-8)
     ((:latin1 :latin-1 :iso-8859-1)
      #+(or (and asdf-unicode (not clisp)) sbcl) :latin1
      #+(and asdf-unicode clisp) charset:iso-8859-1))
   (progn
     (ecase *on-unsupported-encoding*
       ((:error) (cerror "continue using :default" "unsupported encoding ~S" encoding))
       ((:warn) (warn "unsupported encoding ~S, falling back to using :default " encoding))
       ((nil) nil))
     :default)))

(defun register-asdf-encodings ()
  (setf asdf:*encoding-external-format-hook* 'encoding-external-format)
  (values))

(register-asdf-encodings)
