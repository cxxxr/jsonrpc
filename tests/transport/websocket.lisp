(in-package #:cl-user)
(defpackage #:jsonrpc/tests/transport/websocket
  (:use #:cl
        #:rove
        #:jsonrpc)
  (:import-from #:bordeaux-threads))
(in-package #:jsonrpc/tests/transport/websocket)

(deftest websocket-server
  (let ((server-thread
          (bt:make-thread
           (lambda ()
             (let ((server (jsonrpc:make-server)))
               (jsonrpc:expose server "sum" (lambda (args) (reduce #'+ args)))
               (jsonrpc:server-listen server :port 50879 :mode :websocket)))))
        (client (jsonrpc:make-client)))

    (unwind-protect
         (progn
           (sleep 3)
           (jsonrpc:client-connect client :url "ws://127.0.0.1:50879" :mode :websocket)
           (ok (= (jsonrpc:call client "sum" '(10 20)) 30)))
      (bt:destroy-thread server-thread)
      (jsonrpc:client-disconnect client))))
