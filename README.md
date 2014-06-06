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
(find-urn "SOME:URN")
(find-urns-for-url "http://foo.bar.com")
```

Dependencies
------------

* drakma
* cxml
* lisp-unit (for the tests only)

Testing
-------
To run this test suite you need an appropriate mockservice (not available in the repository yet).
The test suite makes use of the [lisp-unit](http://www.cliki.net/lisp-unit) framework.
It is available from quicklisp, e.g.:
```lisp
(ql:quickload :lisp-unit)
```

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
