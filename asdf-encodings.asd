;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :asdf-encodings
  :description "Portable interface to character encodings and your implementation's external-format"
  :depends-on ((:version "asdf" "2.20.17"))
  :components
  ((:file "pkgdcl")
   (:file "encodings" :depends-on ("pkgdcl"))
   (:file "autodetect" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("pkgdcl"))
   (:file "initialization" :depends-on ("pkgdcl"))))


(defmethod perform ((op test-op) (system (eql (find-system :asdf-encodings))))
  (asdf:load-system :asdf-encodings-test)
  (funcall (asdf::find-symbol* :test-suite :asdf-encodings-test)))
