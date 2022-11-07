(defsystem "jsonrpc"
  :class :package-inferred-system
  :version "0.3.2"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "JSON-RPC 2.0 server/client implementation"
  :depends-on ("jsonrpc/main")
  :in-order-to ((test-op (test-op "jsonrpc/tests"))))

(asdf:register-system-packages "clack-handler-hunchentoot" '(#:clack.handler.hunchentoot))
(asdf:register-system-packages "lack-request" '(#:lack.request))
(asdf:register-system-packages "http-body" '(#:http-body.util))

(defsystem "jsonrpc/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "jsonrpc/tests/request-response"
               "jsonrpc/tests/transport/tcp"
               "jsonrpc/tests/transport/stdio"
               "jsonrpc/tests/transport/websocket")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
