# jsonrpc

[![Quicklisp dist](http://quickdocs.org/badge/jsonrpc.svg)](http://quickdocs.org/jsonrpc/)
[![Build Status](https://travis-ci.org/fukamachi/jsonrpc.svg?branch=master)](https://travis-ci.org/fukamachi/jsonrpc)
[![Coverage Status](https://coveralls.io/repos/fukamachi/jsonrpc/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/jsonrpc)

JSON-RPC 2.0 server/client for Common Lisp.

## Usage

```common-lisp
;; server
(defvar *server* (jsonrpc:make-server))
(jsonrpc:expose *server* "sum" (lambda (args) (reduce #'+ args)))

(jsonrpc:server-listen *server* :port 50879 :mode :tcp)
```

```common-lisp
;; client
(defvar *client* (jsonrpc:make-client))
(jsonrpc:client-connect *client* :url "http://127.0.0.1:50879" :mode :tcp)
(jsonrpc:call *client* "sum" '(10 20))
;=> 30

;; Calling with :timeout option
(jsonrpc:call *client* "sum" '(10 20) :timeout 1.0)
;=> 30
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
