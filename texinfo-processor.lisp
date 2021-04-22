(defpackage :webinfo/texinfo-processor
  (:use :cl))

(in-package :webinfo/texinfo-processor)

;; @clfunction{alexandria:flatten}
;; @clmacro{cl:with-open-file}

(defun process-texinfo-file (file)
  "Expands @clfunction, @clmacro, etc. definitions to a Texinfo definition with body extracted from Common Lisp code")

;; an idea could be to use the following via an @clpackage macro that expands to a full Texinfo chapter with definitions
(defun generate-texinfo-reference-for-package (package stream)
  "Generates a Texinfo reference with PACKAGE external symbols documentation")
