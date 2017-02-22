# jsonrpc

[![Quicklisp dist](http://quickdocs.org/badge/jsonrpc.svg)](http://quickdocs.org/jsonrpc/)

JSON-RPC 2.0 server/client for Common Lisp.

## Usage

```common-lisp
;; server
(defvar *server* (jsonrpc:make-server))
(jsonrpc:register-method *server* "sum" (lambda (args) (reduce #'+ args)))

(jsonrpc:server-listen *server* :port 50879 :mode :tcp)
```

```common-lisp
;; client
(defvar *client* (jsonrpc:make-client))
(jsonrpc:client-connect *client* :url "http://127.0.0.1:50879")
(jsonrpc:call *client* "sum" '(10 20))
;=> 30
```

## Extension

### WebSocket transport

```common-lisp
(ql:quickload :jsonrpc-websocket)

(defvar *server* (jsonrpc:make-server))
(jsonrpc:register-method *server* "sum" (lambda (args) (reduce #'+ args)))
(jsonrpc:server-listen *server* :port 50879 :mode :websocket)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
