(in-package :nb-urn-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct url-info
  last-valid-timestamp
  mime-type
  status
  status-timestamp
  url
  )

(defstruct urn-info
  default-url
  url-list
  urn
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SMALL HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun soap-content (elm)
  (if (dom:has-child-nodes elm)
      (let ((first-child (dom:first-child elm)))
	(if (and (dom:has-child-nodes first-child)
		 (eql :text (dom:node-type (dom:first-child first-child))))
	    (dom:node-value (dom:first-child first-child))))))


(defun soap-element (elm tag-name)
  (let ((child-elm (dom:get-elements-by-tag-name elm tag-name)))
    (if (> (length child-elm) 0)
	(if (> (length child-elm) 1)
	    child-elm
	    (aref child-elm 0)))))

(defun soap-element-has-value-p (elm)
  (and (dom:has-child-nodes elm)
       (dom:has-child-nodes (dom:first-child elm))))


;; (defun soap-element-value (elm)
;;   (if (soap-element-has-value-p elm)
;;       (dom:first-child elm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETURNS NODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun soap-header (envelope)
  (aref (dom:get-elements-by-tag-name-ns
	 envelope
	 "http://schemas.xmlsoap.org/soap/envelope/"
	 "Header")
	0))

(defun soap-body (envelope)
  (aref (dom:get-elements-by-tag-name-ns
	 envelope
	 "http://schemas.xmlsoap.org/soap/envelope/"
	 "Body")
	0))

(defun soap-envelope (response)
  (aref (dom:get-elements-by-tag-name-ns
	 response
	 "http://schemas.xmlsoap.org/soap/envelope/"
	 "Envelope")
	0))

(defun soap-return (envelope)
  (let ((ret (dom:get-elements-by-tag-name envelope "return")))
    (if (> (dom:length ret) 0)
	(aref ret 0))))

(defun soap-url-list (return-node)
  (let ((url-list (dom:get-elements-by-tag-name return-node "urlList")))
    (if (> (dom:length url-list) 0)
    	(loop for url across (dom:child-nodes (aref url-list 0))
	   collect (soap-url-info url)))))

(defun soap-login-response (envelope)
  (aref
   (dom:get-elements-by-tag-name-ns
    (soap-body envelope) "http://nb.no/idservice/v1.0/" "loginResponse")
   0))

(defun soap-sso-token (envelope)
   (soap-element-value
    (soap-element (soap-login-response envelope)
		  "SSOToken")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETURNS ACTUAL VALUES, NOT NODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun soap-element-value (elm)
  (if (and (dom:has-child-nodes elm)
	   (eql (dom:node-type (dom:first-child elm)) :text))
      (dom:node-value (dom:first-child elm))))

(defun soap-urn-info (urn-elm)
  (let ((default-url (soap-element urn-elm "defaultURL"))
	(url-list (soap-url-list urn-elm))
	(urn (soap-element urn-elm "URN")))
    (make-urn-info
     :default-url (soap-element-value default-url)
     :url-list url-list
     :urn (soap-element-value urn))))

(defun soap-url-info (url-elm)
  (let ((last-valid-timestamp (soap-element url-elm "lastValidTimestamp"))
	(mime-type (soap-element url-elm "mimeType"))
	(status (soap-element url-elm "status"))
	(status-timestamp (soap-element url-elm "statusTimestamp"))
	(url (soap-element url-elm "URL")))
    (make-url-info
     :last-valid-timestamp
     (soap-element-value last-valid-timestamp)
     :mime-type
     (soap-element-value mime-type)
     :status
     (soap-element-value status)
     :status-timestamp
     (soap-element-value status-timestamp)
     :url
     (soap-element-value url))))

(defun soap-default-url (envelope)
  (dom:node-value
   (dom:first-child
    (aref (dom:get-elements-by-tag-name (soap-return envelope) "defaultURL")
	  0))))





