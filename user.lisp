(defpackage :webinfo-user
  (:use :cl)
  (:export :make-webinfo))

(in-package :webinfo-user)

(defun make-webinfo (filepath output-filepath &rest args)
  "Compiles a Texinfo XML file to a Webinfo file.

See: texi2any --xml"
  (let ((doc (apply #'make-instance 'webinfo::xml-info-document
                    :filepath filepath
                    args)))
    (webinfo::write-info-document-to-file doc output-filepath)))
