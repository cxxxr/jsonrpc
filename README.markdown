# jsonrpc

JSON-RPC 2.0 server/client for Common Lisp.

## Usage

```common-lisp
;; server
(defvar *mapper* (jsonrpc:make-mapper))
(jsonrpc:register-method *mapper* "sum" (lambda (&rest args) (reduce #'+ args)))

(jsonrpc:server-listen *mapper* :port 50879)
```

```common-lisp
;; client
(let ((client (jsonrpc:client-connect :host "127.0.0.1" :port 50879)))
  (jsonrpc:call client "sum" '(10 20)))
;=> 30
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

TBD
