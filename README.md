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
(initialize-client :username "foo" :password "bar" :endpoint "baz")

;;; Does not require a valid username or password, only an endpoint:
(find-urn "SOME:URN")
(find-urns-for-url "http://foo.bar.com")

;;; Requires an endpoint and valid username and password
(login)
(register-urn "SOME:URN" "http://foo.bar.com")
(logout)
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
```
Optionally, if you want more verbose output:
```lisp
(run-all-tests :verbose t)
```
