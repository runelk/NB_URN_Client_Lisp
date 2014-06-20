Lisp Client for the URN PID service by the National Library of Norway
=======================================================================


Library
-------
The **nb_urn_client** directory contains the library for communicating with the SOAP API.
To load the library, make sure that the `nb-urn-client`-folder is available to ASDF.
You also need user credentials and endpoint information from NB.

### Usage Example:

```lisp
(asdf:load-system :nb-urn-client)
(in-package :nb-urn-client)

;;; Initialize the client with named arguments:
(defparameter *client*
  (make-instance 'nb-urn-client
                 :username "foo"
                 :password "bar"
                 :endpoint "baz")

;;; Or, initialize the client with information in globals.lisp:
(defparameter *client* (make-instance 'nb-urn-client))

;;; You need to login first
(login *client*)

;;; Reserve the next available URN in the given series
(defparameter *new-urn* (reserve-next-urn *client* "some:urn:prefix"))

;;; Register some valid URL for the newly created URN
(add-url *client* (urn-info-urn *new-urn*) "http://www.someurl.com/")

;;; Register another URL for the same URN
(add-url *client* (urn-info-urn *new-urn*) "http://www.someotherurl.com/")

;;; Set one of the registered URLs to be the default URL
(set-default-url *client* (urn-info-urn *new-urn*) "http://www.someurl.com/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALTERNATIVE: Using create-urn
;;; (defparameter *new-urn* 
;;;               (create-urn *client*
;;;                           "some:urn:prefix" 
;;;                           "http://www.someurl.com"))
;;; (add-url *client* "http://www.someotherurl.com/")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Retrieve the URN you just created and have a look
(find-urn *client* (urn-info-urn *new-urn*))

;;; Retrieve all URNs containing the URL you added. Your URN should be in the list
(find-urns-for-urn *client* "http://www.someurl.com/")

;;; Logout when you're done
(logout *client*)
```

Dependencies
------------

* [drakma](http://www.cliki.net/drakma)
* [cxml](http://www.cliki.net/cxml)
* [lisp-unit](http://www.cliki.net/lisp-unit) (for testing)

All dependencies are available in [quicklisp](http://www.quicklisp.org/).

Testing
-------
To run this test suite you need an appropriate mockservice (not available in the repository yet).

Make sure the `nb-urn-client` and `test` folders are available to ASDF.

```lisp
(asdf:load-system :nb-urn-client-tests)
(in-package :nb-urn-client-tests)
(run-all-tests)

;;; Optionally, if you want more verbose output:
(run-all-tests :verbose t)
```
