(in-package #:cl-user)

(asdf:defsystem #:jsonrpc
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
    :depends-on ("request-response" "utils")
    :components
    ((:file "interface")
     (:file "tcp")))
   (:file "utils")
   (:file "errors")))
