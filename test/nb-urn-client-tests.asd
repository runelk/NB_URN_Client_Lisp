(in-package :cl-user)

(asdf:defsystem :nb-urn-client-tests
  :name "nb-urn-client-tests"
  :author "Rune Lain Knudsen"
  :maintainer "Rune Lain Knudsen <rune.knudsen@uib.no>"
  :description "Test suite for the nb-urn-client library"
  :depends-on (:nb-urn-client :lisp-unit)
  :components ((:file "nb-urn-client-tests")))
