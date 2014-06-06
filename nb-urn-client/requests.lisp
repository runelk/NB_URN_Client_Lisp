(in-package :nb-urn-client)

(defun soap-has-fault-p (soap-document)
  (> (dom:length (dom:get-elements-by-tag-name-ns
		  soap-document
		  "http://schemas.xmlsoap.org/soap/envelope/"
		  "Fault"))
     0))

(defun send-request (envelope &key endpoint)
  (let ((request-result (drakma:http-request (or endpoint *default-endpoint* )
					     :method :post
					     :content-type "text/xml;charset=UTF-8"
					     :content envelope)))
    (unless (null-or-empty-string-p request-result)
	(let ((document (cxml:parse request-result
				    (cxml-dom:make-dom-builder))))
	  (when (soap-has-fault-p document)
	    (let ((fault (first (soap-faults document))))
	      (error 'soap-fault-error
		     :faultcode (soap-fault-faultcode fault)
		     :faultstring (soap-fault-faultstring fault)
		     :detail (soap-fault-detail fault))))
	  document))))

:eof
