(in-package :nb-urn-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN CLIENT STRUCT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct urn-client
  username
  password
  wsdl ; NB: Not in use for now
  endpoint
  sso-token
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((client nil))

  (defun initialize-client (&key username password wsdl endpoint)
    (setf client (make-urn-client :username (or username *default-username*)
				  :password (or password *default-password*)
				  :wsdl (or wsdl *default-wsdl-uri*)
				  :endpoint (or endpoint *default-endpoint*))))

  (defun sso-token () (urn-client-sso-token client))

  ;; Add a new URL that is to be associated with the specified URN.
  ;;
  ;; Keyword arguments:
  ;;     urn -- the URN to associate the new target to 
  ;;     url -- the URL pointing to the target
  (defun add-url (urn url)
    (if (sso-token)
	(let ((result (send-request (generate-add-url (sso-token) urn url)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Create a new URN under the specified series/prefix.
  ;;
  ;; Keyword arguments:
  ;;     series_code -- the series code / prefix under which to create a new URN
  ;;     url -- the URL pointing to the target of the URN
  ;;
  ;; NB: This will only work if:
  ;; - the ID service supports assignment of serial numbers for the specified series, and
  ;; - if the user has access to this series.
  ;; The created URN is stored in the ID service.
  (defun create-urn (series-code url)
    (if (sso-token)
	(let ((result (send-request (generate-create-urn (sso-token) series-code url)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Delete a URL pointing to a target associated with the given URN.
  ;;
  ;; Keyword arguments:
  ;;     urn -- the URN associated with the URL to delete
  ;;     url -- The URL to delete
  ;;
  ;; This operation is only allowed as long as there is more than
  ;; one registered target for the specified URN.  
  (defun delete-url (urn url)
    (if (sso-token)
	(let ((result (send-request (generate-delete-url (sso-token) urn url)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Find a registered URN with all corresponding locations, along with
  ;; other registered information regarding the URN.
  (defun find-urn (urn)
    (soap-urn-info
     (soap-return
      (soap-envelope
       (send-request (generate-find-urn urn)
		     :endpoint (urn-client-endpoint client))))))

  (defun find-urns-for-url (url)
    (soap-urn-list
     (soap-return
      (soap-envelope
       (send-request (generate-find-urns-for-url url)
		     :endpoint (urn-client-endpoint client))))))

  ;; Request the next available URN from a series/prefix.
  ;;
  ;; Keyword arguments:
  ;;     series_code -- a string containing the series code / prefix
  ;;
  ;; NB: This will only work if:
  ;; - the ID service supports assignment of serial numbers for the specified series, and
  ;; - if the user has access to this series.
  ;; The returned URN is not stored in the ID service.  
  (defun get-next-urn (series-code)
    (if (sso-token)
	(let ((result (send-request (generate-get-next-urn (sso-token) series-code)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Used to log in to the ID-service.
  ;;
  ;; Keyword arguments:
  ;;     username -- a valid username string
  ;;     password -- a valid password string
  ;;
  ;; If no username and/or password is supplied to the method, the
  ;; client tries to find the missing information from the global parameters.
  ;; (see globals.lisp)
  ;; config file.
  (defun login (&key username password)
    (let* ((username (or username (urn-client-username client)))
	   (password (or password (urn-client-password client)))
	   (new-sso-token (soap-sso-token
			  (send-request (generate-login username password)
					:endpoint (urn-client-endpoint client)))))
      (unless (null new-sso-token)
	(setf (urn-client-sso-token client) new-sso-token)))
    (sso-token))

  ;; Used to log out of the ID-service.
  (defun logout ()
    (unless (null (sso-token))
      (send-request (generate-logout (sso-token))
		    :endpoint (urn-client-endpoint client))
      (setf (urn-client-sso-token client) nil)))

  ;; Register a new URN and attach it to a target pointed to by the URL.
  ;;
  ;; Keyword arguments:
  ;;     urn -- the URN to register
  ;;     url -- the URL pointing to the target of the URN
  ;;
  ;; The URN is stored in the ID service.
  (defun register-urn (urn url)
    (if (sso-token)
	(let ((result (send-request (generate-register-urn (sso-token) urn url)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Replace the location of an existing target identified with the specified URN.
  ;;
  ;; Keyword arguments:
  ;;     urn -- The URN whose target to replace
  ;;     old_url -- The old URL to be replaced
  ;;     new_url -- The new URL to replace the old URL with 
  (defun replace-url (urn old-url new-url)
    (if (sso-token)
	(let ((result (send-request (generate-replace-url (sso-token) urn old-url new-url)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Create a new URN under the specified series/prefix and reserve it for future use.
  ;;
  ;; Keyword arguments:
  ;;     series_code -- the series code / prefix under which to reserve a new URN
  ;;
  ;; NB: This will only work if:
  ;; - the ID service supports assignment of serial numbers for the specified series, and
  ;; - if the user has access to this series.
  ;; The created URN is stored in the ID service, but is not attached to any locations.  
  (defun reserve-next-urn (series-code)
    (if (sso-token)
	(let ((result (send-request (generate-reserve-next-urn (sso-token) series-code)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Reserve a URN for future use, without assigning any targets.
  ;;
  ;; Keyword arguments:
  ;;     urn -- the URN to reserve
  ;;
  ;; The URN is stored in the ID service without any associated targets.
  ;; This is only allowed for URNs belonging to a series without serial numbers.
  (defun reserve-urn (urn)
    (if (sso-token)
	(let ((result (send-request (generate-reserve-urn (sso-token) urn)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Set a default URL for a URN.
  ;;
  ;; Keyword arguments:
  ;;     urn -- the URN to set a default URL for
  ;;     url -- the default URL to set for the URN
  ;; The specified URL must be one that is already registered for the URN.
  (defun set-default-url (urn url)
    (if (sso-token)
	(let ((result (send-request (generate-set-default-url (sso-token) urn url)
				    :endpoint (urn-client-endpoint client))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  ;; Retrieve all series available for the current session. 
  ;; The retrieved objecs contain all known information about the series.
  ;; This call is currently unimplemented on the server side.
  (defun get-all-urn-series ()
    'unimplemented-by-server
    ;; (send-request (generate-get-all-urn-series (sso-token))
    ;; 		  :endpoint (urn-client-endpoint client))
    )

  ;; Returns the current API version.
  ;; This call is currently unimplemented on the server side.
  (defun get-version ()
    'unimplemented-by-server
    ;; (send-request (generate-get-version)
    ;; 		  :endpoint (urn-client-endpoint client))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:eof
