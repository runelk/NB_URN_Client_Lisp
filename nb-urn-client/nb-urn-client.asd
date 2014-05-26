
(in-package :cl-user)

(asdf:defsystem :nb-urn-client
  :name "nb-urn-client"
  :author "Rune Lain Knudsen"
  :maintainer "Rune Lain Knudsen <rune.knudsen@uib.no>"
  :description "Lisp client for the URN PID service at Nasjonalbiblioteket (http://idtjeneste.nb.no)"
  :depends-on (:drakma :cxml)
  :components ((:file "package")
	       (:file "soap-generators")
	       (:file "requests")
	       (:file "nb-urn-client"))
  :serial t)

:eof
