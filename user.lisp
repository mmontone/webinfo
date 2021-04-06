(defpackage :webinfo-user
  (:use :cl :cl-fad)
  (:export :make-webinfo))

(in-package :webinfo-user)

(defun make-webinfo (filepath output-path &rest args)
  "Compiles a Texinfo XML file to a Webinfo file.
OUTPUT-PATH can be either a filename path or a directory path.

See: texi2any --xml"
  (let ((output-filepath (if (directory-pathname-p output-path)
                             (merge-pathnames (format nil "~a.winfo" (pathname-name filepath))  output-path)
                             output-path)))
    (let ((doc (apply #'make-instance 'webinfo::xml-info-document
                      :filepath filepath
                      args)))
      (webinfo::write-info-document-to-file doc output-filepath))))