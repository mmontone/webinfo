;;;; package.lisp

(defpackage #:webinfo
  (:use #:cl #:anaphora #:assoc-utils)
  (:export
   :start-webinfo
   :info-document
   :info-node
   :xml-info-document))
