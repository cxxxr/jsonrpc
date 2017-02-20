(in-package #:cl-user)

(asdf:defsystem #:jsonrpc
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "JSON-RPC 2.0 server/client implementation"
  :depends-on (#:yason
               #:usocket
               #:fast-io
               #:trivial-utf-8
               #:alexandria
               #:uiop)
  :components
  ((:file "main" :depends-on ("server" "client" "transports" "request-response" "server/mapper" "errors" "utils"))
   (:file "server" :depends-on ("transports" "server/mapper"))
   (:file "server/mapper" :depends-on ("request-response" "errors"))
   (:file "client" :depends-on ("transports"))
   (:file "request-response" :depends-on ("errors"))
   (:file "transports" :depends-on ("transport"))
   (:module "transport"
    :serial t
    :depends-on ("request-response" "utils" "errors")
    :components
    ((:file "interface")
     (:file "tcp")
     (:file "stdio")))
   (:file "utils")
   (:file "errors")))
