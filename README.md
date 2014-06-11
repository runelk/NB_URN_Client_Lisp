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

;;; Does not require a valid username or password, only an endpoint:
(find-urn *client* "SOME:URN")
(find-urns-for-url *client* "http://foo.bar.com")

;;; Requires an endpoint and valid username and password
(login *client*)
(register-urn *client* "SOME:URN" "http://foo.bar.com")
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
