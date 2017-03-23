#-asdf3.1 (error "jsonrpc requires ASDF 3.1")
(asdf:defsystem #:jsonrpc
  :class :package-inferred-system
  :version "0.3.2"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "JSON-RPC 2.0 server/client implementation"
  :depends-on ("jsonrpc/main")
  :in-order-to ((test-op (test-op jsonrpc/tests))))

(asdf:register-system-packages "clack-handler-hunchentoot" '(#:clack.handler.hunchentoot))
