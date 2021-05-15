;;;; package.lisp

(defpackage #:webinfo
  (:use #:cl #:anaphora #:assoc-utils)
  (:export
   :start-webinfo
   :info-document
   :info-node
   :xml-info-document
   :sexp-info-document
   :winfo-info-document

   :info-repository
   :file-info-repository
   :dir-info-repository

   :nav-theme

   :make-memory-search-index
   :make-persistent-search-index))
