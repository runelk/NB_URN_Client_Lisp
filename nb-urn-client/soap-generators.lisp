(in-package :nb-urn-client)

(defmacro generate-envelope (body)
  `(cxml:with-xml-output (cxml:make-string-sink) ;; :indentation 2 :canonical nil
     (cxml:with-namespace ("soapenv" "http://schemas.xmlsoap.org/soap/envelope/")
       (cxml:with-namespace ("v1" "http://nb.no/idservice/v1.0/")
	 (cxml:with-element "soapenv:Envelope"
	   (cxml:with-element "soapenv:Header")
	   (cxml:with-element "soapenv:Body" ,body))))))

(defun generate-add-url (sso-token urn url)
  (generate-envelope
   (cxml:with-element "v1:addURL"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "URN" (cxml:text urn))
     (cxml:with-element "URL" (cxml:text url)))))

(defun generate-create-urn (sso-token series-code url)
  (generate-envelope
   (cxml:with-element "v1:createURN"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "seriesCode" (cxml:text series-code))
     (cxml:with-element "URL" (cxml:text url)))))

(defun generate-delete-url (sso-token urn url)
  (generate-envelope
   (cxml:with-element "v1:deleteURL"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "URN" (cxml:text urn))
     (cxml:with-element "URL" (cxml:text url)))))

(defun generate-find-urn (urn)
  (generate-envelope
   (cxml:with-element "v1:findURN"
     (cxml:with-element "URN" (cxml:text urn)))))

(defun generate-find-urns-for-url (url)
  (generate-envelope
   (cxml:with-element "v1:findURNsForURL"
     (cxml:with-element "URL" (cxml:text url)))))

(defun generate-get-next-urn (sso-token series-code)
  (generate-envelope
   (cxml:with-element "v1:getNextURN"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "seriesCode" (cxml:text series-code)))))

(defun generate-login (username password)
  (generate-envelope
   (cxml:with-element "v1:login"
     (cxml:with-element "username" (cxml:text username))
     (cxml:with-element "password" (cxml:text password)))))

(defun generate-logout (sso-token)
  (generate-envelope
   (cxml:with-element "v1:SSOToken" (cxml:text sso-token))))

(defun generate-register-urn (sso-token urn url)
  (generate-envelope
   (cxml:with-element "v1:registerURN"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "URN" (cxml:text urn))
     (cxml:with-element "URL" (cxml:text url)))))

(defun generate-replace-url (sso-token urn old-url new-url)
  (generate-envelope
   (cxml:with-element "v1:replaceURL"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "URN" (cxml:text urn))
     (cxml:with-element "oldURL" (cxml:text old-url))
     (cxml:with-element "newURL" (cxml:text new-url)))))

(defun generate-reserve-next-urn (sso-token series-code)
  (generate-envelope
   (cxml:with-element "v1:reserveNextURN"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "seriesCode" (cxml:text series-code)))))

(defun generate-reserve-urn (sso-token urn)
  (generate-envelope
   (cxml:with-element "v1:reserveURN"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "URN" (cxml:text urn)))))

(defun generate-set-default-url (sso-token urn url)
  (generate-envelope
   (cxml:with-element "v1:setDefaultURL"
     (cxml:with-element "SSOToken" (cxml:text sso-token))
     (cxml:with-element "URN" (cxml:text urn))
     (cxml:with-element "URL" (cxml:text url)))))

(defun generate-get-all-urn-series () 'unimplemented-by-server)
(defun generate-get-version () 'unimplemented-by-server)


:eof
