;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

;; Ensure upgrade, so #+asdf-unicode below has better chances of working.
(asdf:oos 'asdf:load-op :asdf)

(asdf:defsystem :asdf-encodings
  :description "Portable interface to character encodings and your implementation's external-format"
  :depends-on ((:version "asdf" "2.20.18"))
  #+asdf-unicode #+asdf-unicode ;; disabled in implementations with no unicode support.
  :components
  ((:file "pkgdcl")
   (:file "encodings" :depends-on ("pkgdcl"))
   (:file "autodetect" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("pkgdcl"))
   (:file "initialization" :depends-on ("pkgdcl"))))

(defmethod perform ((op test-op) (system (eql (find-system :asdf-encodings))))
  (asdf:load-system :asdf-encodings-test)
  (funcall (asdf::find-symbol* :test-suite :asdf-encodings-test)))
