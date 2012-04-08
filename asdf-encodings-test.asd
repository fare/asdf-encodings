;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :asdf-encodings-test
  :depends-on (:asdf-encodings :fare-utils :hu.dwim.stefil)
  :components
  ((:file "asdf-encodings-test")))
