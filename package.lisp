;;;; package.lisp

(defpackage #:webinfo
  (:use #:cl #:anaphora #:assoc-utils #:cl-fad)
  (:export
   :start-webinfo
   :info-document
   :info-node
   :xml-info-document))
