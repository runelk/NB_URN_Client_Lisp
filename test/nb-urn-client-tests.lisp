(in-package :cl-user)

(defpackage :nb-urn-client-tests
  (:use :common-lisp
	:lisp-unit
	:nb-urn-client))

(in-package :nb-urn-client-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *mock-username* "localuser")
(defparameter *mock-password* "localpassword")
(defparameter *mock-wsdl-uri* "http://localhost:8088/idservice?WSDL")
(defparameter *mock-endpoint* "http://localhost:8088/idservice?ws")
(defparameter *mock-sso-token* "jf8d09asmockfud089as8u")

(defun setup ()
  (setf nb-urn-client::*default-username* *mock-username*)
  (setf nb-urn-client::*default-password* *mock-password*)
  (setf nb-urn-client::*default-wsdl-uri* *mock-wsdl-uri*)
  (setf nb-urn-client::*default-endpoint* *mock-endpoint*)
  
  (defparameter *client* (make-instance 'nb-urn-client
					:username *mock-username*
					:password *mock-password*
					:wsdl *mock-wsdl-uri*
					:endpoint *mock-endpoint*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test test-add-url)

(define-test test-create-urn)

(define-test test-delete-url)

(define-test test-find-urn
  (let* ((urn-existing "URN:NBN:no-nb_ClarinoPrefix1_1234_1234")
	 (urn-non-existing "URN:NBN:no-nb_ClarinoPrefix1_1234_1235")
	 (url "http://www.aftenposten.com/")
	 (result (nb-urn-client:find-urn *client* urn-existing)))
    (assert-error 'nb-urn-client::soap-fault-error
		  (nb-urn-client:find-urn *client* urn-non-existing))
    (assert-equal url (nb-urn-client:urn-info-default-url result))
    (assert-equal 1 (length (nb-urn-client:urn-info-url-list result)))
    (assert-equal urn-existing (nb-urn-client:urn-info-urn result))))

(define-test test-find-urns-for-url
  (let* ((url-existing "http://www.aftenposten.no/")
	 (url-non-existing "http://www.foo.com/")
	 (result-non-existing (nb-urn-client:find-urns-for-url *client* url-non-existing))
	 (result-existing (nb-urn-client:find-urns-for-url *client* url-existing)))
    (assert-true (null result-non-existing))
    (assert-equal 2 (length result-existing))))

(define-test test-get-next-urn)

(define-test test-login
  (assert-error 'nb-urn-client::soap-fault-error
		(nb-urn-client:login *client* :username "foo" :password "bar"))
  (let ((result (nb-urn-client:login *client*)))
    (assert-equal *mock-sso-token* result)
    (assert-equal *mock-sso-token* (nb-urn-client:sso-token *client*)))
  (nb-urn-client:logout *client*))

(define-test test-logout
  (assert-true (null (nb-urn-client:sso-token *client*)))
  (nb-urn-client:login *client*)
  (assert-false (null (nb-urn-client:sso-token *client*)))
  (nb-urn-client:logout *client*)
  (assert-true (null (nb-urn-client:sso-token *client*))))

(define-test test-register-urn)

(define-test test-replace-url)

(define-test test-reserve-next-urn)

(define-test test-reserve-urn)

(define-test test-set-default-url)

(define-test test-get-all-urn-series)

(define-test test-get-version)

(defun run-all-tests (&key verbose)
  (setf *print-summary* verbose)
  (setf *print-failures* verbose)
  (setf *print-errors* verbose)
  (setup)
  (run-tests))

