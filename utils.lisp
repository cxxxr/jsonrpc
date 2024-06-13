(defpackage #:jsonrpc/utils
  (:use #:cl)
  (:import-from #:usocket
                #:socket-listen
                #:socket-close
                #:address-in-use-error)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:hash-exists-p
           #:random-port
           #:make-id))
(in-package #:jsonrpc/utils)

(defun hash-exists-p (hash-table key)
  (nth-value 1 (gethash key hash-table)))

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defun make-id (&optional (length 12))
  (declare (type fixnum length))
  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))
