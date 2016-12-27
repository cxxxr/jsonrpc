(in-package #:cl-user)

(asdf:defsystem #:jsonrpc
  :depends-on (#:yason
               #:usocket
               #:fast-io
               #:trivial-utf-8
               #:alexandria
               #:uiop)
  :components
  ((:file "main" :depends-on ("server" "request-response" "errors"))
   (:file "server" :depends-on ("handlers"))
   (:file "request-response" :depends-on ("errors"))
   (:file "handlers" :depends-on ("handler"))
   (:module "handler"
    :serial t
    :depends-on ("request-response" "utils")
    :components
    ((:file "interface")
     (:file "tcp")))
   (:file "utils")
   (:file "errors")))
