(in-package :cl-user)

(defpackage :asdf-encodings
  (:use :cl)
  (:export
   #:encoding-external-format
   #:*on-unsupported-encoding*))

(in-package :asdf-encodings)

(defvar *on-unsupported-encoding* :error
  "One of :error, :warn or nil, specifies what to do when passed an unsupported encoding.")

(defvar *encoding-aliases*
  (loop :with h = (make-hash-table)
    :for (name . aliases) :in
    ;; Define valid names for an encoding.
    ;; When an implementation doesn't recognize "our" normalized name,
    ;; pick a name that the implementation likes.
    '((:default)
      (:utf-8 :utf8)
      (#+ecl :us-ascii :ascii :us-ascii)
      (:latin1 :latin-1 :iso-8859-1 :iso8859-1))
    :do (dolist (s (cons name aliases)) (setf (gethash s h) name))
    :finally (return h)))

(defun normalize-encoding (encoding)
  (gethash encoding *encoding-aliases*))

(defun encoding-external-format (encoding &key (on-error *on-unsupported-encoding*))
  (or (ignore-errors (encoding-external-format* encoding))
      (progn
        (ecase on-error
          ((:error) (cerror "continue using :default" "unsupported encoding ~S" encoding))
          ((:warn) (warn "unsupported encoding ~S, falling back to using :default " encoding))
          ((nil) nil))
        :default)))

(defun encoding-external-format* (encoding)
  (let ((encoding (normalize-encoding encoding)))
    #+allegro (excl:find-external-format encoding)
    #+ccl (ignore-errors (ccl::normalize-external-format t encoding))
    #+clisp (asdf:find-symbol* encoding :charset)
    #+(or cmu scl) (stream::find-external-format encoding)
    #+ecl (ignore-errors (ext:make-encoding encoding))
    #+sbcl (sb-impl::get-external-format encoding)
    #-(or allegro ccl clisp cmu ecl sbcl scl) encoding))

(defun register-asdf-encodings ()
  (setf asdf:*encoding-external-format-hook* 'encoding-external-format)
  (values))

(register-asdf-encodings)
