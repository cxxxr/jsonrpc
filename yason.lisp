(defpackage #:jsonrpc/yason
  (:use #:cl)
  (:export #:*parse-object-key-fn*
           #:*parse-object-as*
           #:*parse-json-arrays-as-vectors*
           #:*parse-json-booleans-as-symbols*
           #:*parse-json-null-as-keyword*
           #:parse))
(in-package #:jsonrpc/yason)

(defparameter *parse-object-key-fn* yason:*parse-object-key-fn*)
(defparameter *parse-object-as* yason:*parse-object-as*)
(defparameter *parse-json-arrays-as-vectors* yason:*parse-json-arrays-as-vectors*)
(defparameter *parse-json-booleans-as-symbols* yason:*parse-json-booleans-as-symbols*)
(defparameter *parse-json-null-as-keyword* yason:*parse-json-null-as-keyword*)

(defun parse (input)
  (yason:parse input
               :object-key-fn *parse-object-key-fn*
               :object-as *parse-object-as*
               :json-arrays-as-vectors *parse-json-arrays-as-vectors*
               :json-booleans-as-symbols *parse-json-booleans-as-symbols*
               :json-nulls-as-keyword *parse-json-null-as-keyword*))
