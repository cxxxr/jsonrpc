# jsonrpc

JSON-RPC 2.0 server/client for Common Lisp.

## Usage

```common-lisp
;; server
(jsonrpc:server-listen
  (lambda (request)
    (cond
      ((= (jsonrpc:request-method request) "textDocument/didChange")
       ...
       (let ((response (jsonrpc:make-response :id (jsonrpc:request-id request)
                                              :result xxx)))
         (jsonrpc:send-message response)))
      ...))
  :port 50879)
```

```common-lisp
;; client
(let ((transport (jsonrpc:client-connect :host "127.0.0.1" :port 50879)))
  (jsonrpc:call transport "textDocument/didChange"))
```

```
# client (telnet)
$ telnet 127.0.0.1 50879
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Content-Length: 74

{ "jsonrpc": "2.0","id": 1,"method": "textDocument/didOpen","params": [] }
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

TBD
