(in-package :nb-urn-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN CLASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nb-urn-client ()
  ((username :accessor username :initarg :username :initform *default-username*)
   (password :accessor password :initarg :password :initform *default-password*)
   (wsdl :accessor wsdl :initarg :wsdl :initform *default-wsdl-uri*)
   (endpoint :accessor endpoint :initarg :endpoint :initform *default-endpoint*)
   (sso-token :accessor sso-token :initarg :sso-token :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASS HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-sso-token ((client nb-urn-client))
  (unless (sso-token client)
    (error 'nb-urn-client-error :what "No SSO token available.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add a new URL that is to be associated with the specified URN.
;;;
;;; Keyword arguments:
;;;     urn -- the URN to associate the new target to 
;;;     url -- the URL pointing to the target
(defmethod add-url ((client nb-urn-client) urn url)
  (let ((result (send-request (generate-add-url (sso-token client) urn url)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Create a new URN under the specified series/prefix.
;;;
;;; Keyword arguments:
;;;     series_code -- the series code / prefix under which to create a new URN
;;;     url -- the URL pointing to the target of the URN
;;;
;;; NB: This will only work if:
;;; - the ID service supports assignment of serial numbers for the specified series, and
;;; - if the user has access to this series.
;;; The created URN is stored in the ID service.
(defmethod create-urn ((client nb-urn-client) series-code url)
  (let ((result (send-request (generate-create-urn (sso-token client) series-code url)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Delete a URL pointing to a target associated with the given URN.
;;;
;;; Keyword arguments:
;;;     urn -- the URN associated with the URL to delete
;;;     url -- The URL to delete
;;;
;;; This operation is only allowed as long as there is more than
;;; one registered target for the specified URN.  
(defmethod delete-url ((client nb-urn-client) urn url)
  (let ((result (send-request (generate-delete-url (sso-token client) urn url)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Find a registered URN with all corresponding locations, along with
;;; other registered information regarding the URN.
(defmethod find-urn ((client nb-urn-client) urn)
  (soap-urn-info
   (soap-return
    (soap-envelope
     (send-request (generate-find-urn urn)
		   :endpoint (endpoint client))))))

(defmethod find-urns-for-url ((client nb-urn-client) url)
  (soap-urn-list
   (soap-return
    (soap-envelope
     (send-request (generate-find-urns-for-url url)
		   :endpoint (endpoint client))))))

;;; Request the next available URN from a series/prefix.
;;;
;;; Keyword arguments:
;;;     series_code -- a string containing the series code / prefix
;;;
;;; NB: This will only work if:
;;; - the ID service supports assignment of serial numbers for the specified series, and
;;; - if the user has access to this series.
;;; The returned URN is not stored in the ID service.  
(defmethod get-next-urn ((client nb-urn-client) series-code)
  (let ((result (send-request (generate-get-next-urn (sso-token client) series-code)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Used to log in to the ID-service.
;;;
;;; Keyword arguments:
;;;     username -- a valid username string
;;;     password -- a valid password string
;;;
;;; If no username and/or password is supplied to the method, the
;;; client tries to find the missing information from the global parameters.
;;; (see globals.lisp)
;;; config file.
(defmethod login ((client nb-urn-client) &key username password)
  (let* ((username (or username (username client)))
	 (password (or password (password client)))
	 (new-sso-token (soap-sso-token
			 (send-request (generate-login username password)
				       :endpoint (endpoint client)))))
    (unless (null new-sso-token)
      (setf (sso-token client) new-sso-token))))

;;; Used to log out of the ID-service.
(defmethod logout ((client nb-urn-client) )
  (unless (null (sso-token client))
    (send-request (generate-logout (sso-token client))
		  :endpoint (endpoint client))
    (setf (sso-token client) nil)))

;;; Register a new URN and attach it to a target pointed to by the URL.
;;;
;;; Keyword arguments:
;;;     urn -- the URN to register
;;;     url -- the URL pointing to the target of the URN
;;;
;;; The URN is stored in the ID service.
(defmethod register-urn ((client nb-urn-client) urn url)
  (let ((result (send-request (generate-register-urn (sso-token client) urn url)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Replace the location of an existing target identified with the specified URN.
;;;
;;; Keyword arguments:
;;;     urn -- The URN whose target to replace
;;;     old_url -- The old URL to be replaced
;;;     new_url -- The new URL to replace the old URL with 
(defmethod replace-url ((client nb-urn-client) urn old-url new-url)
  (let ((result (send-request (generate-replace-url (sso-token client) urn old-url new-url)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Create a new URN under the specified series/prefix and reserve it for future use.
;;;
;;; Keyword arguments:
;;;     series_code -- the series code / prefix under which to reserve a new URN
;;;
;;; NB: This will only work if:
;;; - the ID service supports assignment of serial numbers for the specified series, and
;;; - if the user has access to this series.
;;; The created URN is stored in the ID service, but is not attached to any locations.  
(defmethod reserve-next-urn ((client nb-urn-client) series-code)
  (let ((result (send-request (generate-reserve-next-urn (sso-token client) series-code)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Reserve a URN for future use, without assigning any targets.
;;;
;;; Keyword arguments:
;;;     urn -- the URN to reserve
;;;
;;; The URN is stored in the ID service without any associated targets.
;;; This is only allowed for URNs belonging to a series without serial numbers.
(defmethod reserve-urn ((client nb-urn-client) urn)
  (let ((result (send-request (generate-reserve-urn (sso-token client) urn)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))

;;; Set a default URL for a URN.
;;;
;;; Keyword arguments:
;;;     urn -- the URN to set a default URL for
;;;     url -- the default URL to set for the URN
;;; The specified URL must be one that is already registered for the URN.
(defmethod set-default-url ((client nb-urn-client) urn url)
  (let ((result (send-request (generate-set-default-url (sso-token client) urn url)
			      :endpoint (endpoint client))))
    (when result (soap-urn-info result))))


;;; Retrieve all series available for the current session. 
;;; The retrieved objecs contain all known information about the series.
;;; This call is currently unimplemented on the server side.
(defmethod get-all-urn-series ((client nb-urn-client))
  'unimplemented-by-server
  ;; (send-request (generate-get-all-urn-series (sso-token client))
  ;; 		  :endpoint (endpoint client))
  )

;;; Returns the current API version.
;;; This call is currently unimplemented on the server side.
(defmethod get-version ((client nb-urn-client))
  'unimplemented-by-server
  ;; (send-request (generate-get-version)
  ;; 		  :endpoint (endpoint client))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SSO TOKEN CHECKS FOR METHODS THAT REQUIRE LOGIN CREDENTIALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-url :before ((client nb-urn-client) urn url)
  (check-sso-token client))

(defmethod create-urn :before ((client nb-urn-client) series-code url)
  (check-sso-token client))

(defmethod delete-url :before ((client nb-urn-client) urn url)
  (check-sso-token client))

(defmethod get-next-urn :before ((client nb-urn-client) series-code)
  (check-sso-token client))

(defmethod register-urn :before ((client nb-urn-client) urn url)
  (check-sso-token client))

(defmethod replace-url :before ((client nb-urn-client) urn old-url new-url)
  (check-sso-token client))

(defmethod reserve-next-urn :before ((client nb-urn-client) series-code)
  (check-sso-token client))

(defmethod reserve-urn :before ((client nb-urn-client) urn)
  (check-sso-token client))

(defmethod set-default-url :before ((client nb-urn-client) urn url)
  (check-sso-token client))

:eof
