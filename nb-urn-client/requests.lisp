(in-package :nb-urn-client)

(defun send-request (envelope)
  (let ((request-result (drakma:http-request "http://www.nb.no/idtjeneste-test/ws" 
					     :method :post
					     :content-type "text/xml;charset=UTF-8"
					     :content envelope)))
    (if request-result
	(cxml:parse request-result
		    (cxml-dom:make-dom-builder)))))

:eof
