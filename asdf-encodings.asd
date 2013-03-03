;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem :asdf-encodings
  :defsystem-depends-on (:asdf #-asdf3 :asdf-driver)
  :description "Portable interface to character encodings and your implementation's external-format"
  :components
  ((:file "pkgdcl")
   (:file "encodings" :depends-on ("pkgdcl"))
   (:file "autodetect" :depends-on ("pkgdcl"))
   (:file "asdf-support" :depends-on ("pkgdcl"))
   (:file "initialization" :depends-on ("pkgdcl"))))

(unless (find-symbol (string :component-encoding) :asdf)
  (error "asdf-encodings requires asdf 2.20.18 or later"))

(defmethod perform ((op test-op) (system (eql (find-system :asdf-encodings))))
  (load-system :asdf-encodings-test)
  (funcall (asdf::find-symbol* :test-suite :asdf-encodings-test)))
