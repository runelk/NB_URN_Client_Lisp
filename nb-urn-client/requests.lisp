(in-package :nb-urn-client)

(defun send-request (envelope)
  (cxml:parse
   (drakma:http-request "http://www.nb.no/idtjeneste-test/ws" 
			:method :post
			:content-type "text/xml;charset=UTF-8"
			:content envelope)
   (cxml-dom:make-dom-builder)))


