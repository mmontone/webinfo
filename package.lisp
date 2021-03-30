;;;; package.lisp

(defpackage #:webinfo
  (:use #:cl #:anaphora)
  (:export
   :start-webinfo
   :info-document
   :info-node
   :xml-info-document))
