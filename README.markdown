# jsonrpc

This library provides JSON-RPC server implementation for Common Lisp.

## Usage

```common-lisp
;; server
(jsonrpc:start-server
  (lambda (request)
    (cond
      ((= (jsonrpc:request-method request) "textDocument/didChange")
       ...
       (let ((response (jsonrpc:make-response :id (jsonrpc:request-id request)
                                              :result xxx)))
         (jsonrpc:send-message response)))
      ...)))
```

```
# client
$ telnet 127.0.0.1 50371
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
