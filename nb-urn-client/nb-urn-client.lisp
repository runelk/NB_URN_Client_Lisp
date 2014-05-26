(in-package :nb-urn-client)

(defparameter *default-wsdl-uri* nil)
(defparameter *default-endpoint* nil)
(defparameter *default-username* nil)
(defparameter *default-password* nil)

(defparameter *sso-token* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-url (urn url)
  (send-request (generate-add-url *sso-token* urn url)))

(defun create-urn (series-code url)
  (send-request (generate-create-urn *sso-token* series-code url)))

(defun delete-url (urn url)
  (send-request (generate-delete-url *sso-token* urn url)))

(defun find-urn (urn)
  (send-request (generate-find-urn urn)))

(defun find-urns-for-url (url)
  (send-request (generate-find-urns-for-url url)))

(defun get-next-urn (series-code)
  (send-request (generate-get-next-urn *sso-token* series-code)))

(defun login (username password)
  (send-request (generate-login username password)))

(defun logout ()
  (send-request (generate-logout *sso-token*)))

(defun register-urn (urn url)
  (send-request (generate-register-urn *sso-token* urn url)))

(defun replace-url (urn old-url new-url)
  (send-request (generate-replace-url *sso-token* urn old-url new-url)))

(defun reserve-next-urn (series-code)
  (send-request (generate-reserve-next-urn *sso-token* series-code)))

(defun reserve-urn (urn)
  (send-request (generate-reserve-urn *sso-token* urn)))

(defun set-default-url (urn url)
  (send-request (generate-set-default-url *sso-token* urn url)))

(defun get-all-urn-series ()
  'unimplemented-by-server
  ;; (send-request (generate-get-all-urn-series *sso-token*))
  )

(defun get-version ()
  'unimplemented-by-server
  ;; (send-request (generate-get-version))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END TOP-LEVEL API CALLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:eof
