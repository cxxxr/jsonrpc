(in-package #:cl-user)
(defpackage #:jsonrpc/tests/transport/tcp
  (:use #:cl
        #:rove
        #:jsonrpc)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*)
  (:import-from #:jsonrpc/transport/tcp)
  (:import-from #:jsonrpc/utils #:destroy-thread*)
  (:import-from #:bordeaux-threads))
(in-package #:jsonrpc/tests/transport/tcp)

(deftest tcp-server
  (let ((server-thread
          (bt:make-thread
           (lambda ()
             (let ((server (jsonrpc:make-server)))
               (jsonrpc:expose server "sum" (lambda (args) (reduce #'+ args)))
               (jsonrpc:server-listen server :port 50879 :mode :tcp)))))
        (client (jsonrpc:make-client)))

    (unwind-protect
         (progn
           (sleep 0.5)
           (jsonrpc:client-connect client :url "http://127.0.0.1:50879" :mode :tcp)
           (ok (= (jsonrpc:call client "sum" '(10 20)) 30)))
      (destroy-thread* server-thread)
      (jsonrpc:client-disconnect client))))
