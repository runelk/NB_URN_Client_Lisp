(in-package :nb-urn-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((sso-token nil))

  (defun add-url (urn url)
    (if sso-token
	(let ((result (send-request (generate-add-url sso-token urn url))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun create-urn (series-code url)
    (if sso-token
	(let ((result (send-request (generate-create-urn sso-token series-code url))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun delete-url (urn url)
    (if sso-token
	(let ((result (send-request (generate-delete-url sso-token urn url))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun find-urn (urn)
    (soap-urn-info (send-request (generate-find-urn urn))))

  (defun find-urns-for-url (url)
    (soap-urn-info (send-request (generate-find-urns-for-url url))))

  (defun get-next-urn (series-code)
    (if sso-token
	(let ((result (send-request (generate-get-next-urn sso-token series-code))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun login (username password)
    (let ((new-sso-token
	   (soap-sso-token
	    (send-request (generate-login username password)))))
      (unless (null new-sso-token)
	(setf sso-token new-sso-token)))
    sso-token)

  (defun logout ()
    (unless (null sso-token)
      (send-request (generate-logout sso-token))))

  (defun register-urn (urn url)
    (if sso-token
	(let ((result (send-request (generate-register-urn sso-token urn url))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun replace-url (urn old-url new-url)
    (if sso-token
	(let ((result (send-request (generate-replace-url sso-token urn old-url new-url))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun reserve-next-urn (series-code)
    (if sso-token
	(let ((result (send-request (generate-reserve-next-urn sso-token series-code))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun reserve-urn (urn)
    (if sso-token
	(let ((result (send-request (generate-reserve-urn sso-token urn))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun set-default-url (urn url)
    (if sso-token
	(let ((result (send-request (generate-set-default-url sso-token urn url))))
	  (when result (soap-urn-info result)))
	'no-sso-token))

  (defun get-all-urn-series ()
    'unimplemented-by-server
    ;; (send-request (generate-get-all-urn-series sso-token))
    )

  (defun get-version ()
    'unimplemented-by-server
    ;; (send-request (generate-get-version))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SMALL TESTS (to be removed/refactored)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-find-urn ()
  (let* ((urn "URN:NBN:no-nb_ClarinoPrefix2_1_30")
	 (found-urn (find-urn urn)))
    found-urn))

    ;; (soap-content found-urn)))


:eof
