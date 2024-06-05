(defpackage #:jsonrpc/tests/transport/websocket
  (:use #:cl
        #:rove
        #:jsonrpc)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*)
  (:import-from #:jsonrpc/transport/websocket)
  (:import-from #:bordeaux-threads))
(in-package #:jsonrpc/tests/transport/websocket)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)))

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
           (sleep 0.5)
           (loop until (server-running-p 50879)
                 do (sleep 0.1))
           (jsonrpc:client-connect client :url "ws://127.0.0.1:50879" :mode :websocket)
           (ok (= (jsonrpc:call client "sum" '(10 20)) 30)))
      (bt:destroy-thread server-thread)
      (jsonrpc:client-disconnect client))))
