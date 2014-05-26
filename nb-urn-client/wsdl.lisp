(in-package :nb-urn-client)

(defun parse-wsdl (uri)
  (cxml:parse (drakma:http-request (drakma:http-request uri))
	      (cxml-dom:make-dom-builder)))

(defun get-wsdl-operations (wsdl-dom)
  (dom:get-elements-by-tag-name wsdl-dom "operation"))

:eof
