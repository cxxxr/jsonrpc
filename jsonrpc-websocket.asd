(asdf:defsystem #:jsonrpc-websocket
  :depends-on (#:jsonrpc
               #:websocket-driver
               #:bordeaux-threads
               #:clack
               #:yason)
  :components
  ((:file "transport/websocket")))
