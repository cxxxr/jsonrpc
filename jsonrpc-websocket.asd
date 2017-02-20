(asdf:defsystem #:jsonrpc-websocket
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "WebSocket transport for JSON-RPC 2.0"
  :depends-on (#:jsonrpc
               #:websocket-driver
               #:bordeaux-threads
               #:clack
               #:yason)
  :components
  ((:file "transport/websocket")))
