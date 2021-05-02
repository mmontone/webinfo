(require :def-properties)

(defpackage :webinfo/texinfo-processor
  (:use :cl :assoc-utils))

(in-package :webinfo/texinfo-processor)

;; This embeds a CL function, reading its structure and documentation from Lisp process:
;; @clfunction{alexandria:flatten}
;; Same for macros and other CL stuff:
;; @clmacro{cl:with-open-file}

(defparameter *texinfo-syntax*
  '((:@clfunction . "@clfunction{(.*):(.*)}")))

(defun process-texinfo-file (file stream)
  "Expands @clfunction, @clmacro, etc. definitions to a Texinfo definition with body extracted from Common Lisp code."
  (with-open-file (f file :direction :input
			  :external-format :utf-8)
    (loop for line := (read-line f nil nil)
	  while line
	  do
	     (when (not
		    (block process
		      (dolist (syntax *texinfo-syntax*)
			(when (ppcre:scan (cdr syntax) line)
			  (process-texinfo-syntax (car syntax) line stream)
			  (return-from process t)))))
	       (write-string line stream)
	       (terpri stream)))))

(defgeneric process-texinfo-syntax (syntax line stream))

(defmethod process-texinfo-syntax ((syntax (eql :@clfunction)) line stream)
  (let ((regex (aget *texinfo-syntax* :@clfunction)))
    (ppcre:do-register-groups (package-name symbol-name)
	(regex line)
      (let* ((function-symbol (intern (string-upcase symbol-name)
				      (or (find-package (string-upcase package-name))
					  (error "Package not found: ~a" package-name))))
	     (function-info (def-properties:function-properties function-symbol)))
	(if (null function-info)
	    (error "Function properties could not be read: ~s" function-symbol)
	    (progn
	      (format stream "@cldefun {~a, ~a, ~a}"
		      package-name symbol-name (aget function-info :args))
	      (terpri stream)
	      (when (aget function-info :documentation)
		(write-string (aget function-info :documentation) stream))
	      (terpri stream)
	      (write-string "@endcldefun" stream)
	      (terpri stream)))))))

;; @clpackage-functions
;; @clpackage-variables
;; @clpackage-classes

;; TODO: make @cldefun reference source code when enabled (use swank location).
;; Source code enabled is indicated with a Texinfo variable.
;; Source code is serialized to a Texinfo node with an anchor for each line @anchor{<filename>L<linename>}
		 

;; an idea could be to use the following via an @clpackage-reference{package-name} macro that expands to a full Texinfo chapter with definitions
(defun generate-texinfo-reference-for-package (package stream)
  "Generates a Texinfo reference with PACKAGE external symbols documentation")

(with-output-to-string (s)
  (process-texinfo-file
   (asdf:system-relative-pathname :webinfo "test/texinfo.texi") s))
