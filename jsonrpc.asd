(in-package #:cl-user)

(asdf:defsystem #:jsonrpc
  :depends-on (#:yason
               #:usocket
               #:fast-io
               #:trivial-utf-8
               #:alexandria
               #:uiop)
  :components
  ((:file "main" :depends-on ("server" "transports" "request-response" "errors"))
   (:file "server" :depends-on ("transports" "utils"))
   (:file "request-response" :depends-on ("errors"))
   (:file "transports" :depends-on ("transport"))
   (:module "transport"
    :serial t
    :depends-on ("request-response")
    :components
    ((:file "interface")
     (:file "tcp")))
   (:file "utils")
   (:file "errors")))
