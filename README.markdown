# jsonrpc

[![Quicklisp dist](http://quickdocs.org/badge/jsonrpc.svg)](http://quickdocs.org/jsonrpc/)

JSON-RPC 2.0 server/client for Common Lisp.

## Usage

```common-lisp
;; server
(defvar *mapper* (jsonrpc:make-mapper))
(jsonrpc:register-method *mapper* "sum" (lambda (args) (reduce #'+ args)))

(jsonrpc:server-listen *mapper* :port 50879)
```

```common-lisp
;; client
(let ((client (jsonrpc:client-connect :host "127.0.0.1" :port 50879)))
  (jsonrpc:call client "sum" '(10 20)))
;=> 30
```

## Extension

### WebSocket transport

```common-lisp
(ql:quickload :jsonrpc-websocket)

(defvar *mapper* (jsonrpc:make-mapper))
(jsonrpc:register-method *mapper* "sum" (lambda (args) (reduce #'+ args)))
(jsonrpc:server-listen *mapper* :port 50879 :mode :websocket)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
