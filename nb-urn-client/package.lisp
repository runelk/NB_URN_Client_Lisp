(in-package :cl-user)

(defpackage :nb-urn-client
  (:use :common-lisp
	:drakma
	:cxml)
  (:export :nb-urn-client
	   :add-url
	   :create-urn
	   :delete-url
	   :find-urn
	   :find-urns-for-url
	   :get-next-urn
	   :login
	   :logout
	   :register-urn
	   :replace-url
	   :reserve-next-urn
	   :reserve-urn
	   :set-default-url
	   :get-all-urn-series
	   :get-version
	   :sso-token
	   :urn-info-default-url
	   :urn-info-url-list
	   :urn-info-urn
	   ))

:eof
