# jsonrpc

This library provides JSON-RPC server implementation for Common Lisp.

## Usage

```common-lisp
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

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

TBD
