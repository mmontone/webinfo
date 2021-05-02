(defpackage webinfo-asdf
  (:use :cl))

(in-package :webinfo-asdf)

(defparameter *quicklisp-controller-directory* #p"~/quicklisp-controller/")
(defvar *failed-asdf-files* nil
  "Contains a list of ASDF files that failed to be loaded and the error, after calling REGISTER-ALL-ASDF-FILES.")
(defparameter *conflictive-asdf-files* '("cl-quakeinfo" "qt-libs" "cl-geocode" "cl-geoip")
  "Some ASDF files cause conflicts when trying to be loaded. These are ignored.")

(defun find-files-do (path pattern function &optional (include-subdirectories t))
  "Find files in PATH using PATTERN. Invokes FUNCTION on found files.
If INCLUDE-SUBDIRECTORIES is T, then work recursively."
  (dolist (file (uiop/filesystem:directory-files path pattern))
    (funcall function file))
  (when include-subdirectories
    (dolist (subdir (uiop/filesystem:subdirectories path))
      (find-files-do subdir pattern function include-subdirectories))))

(defun register-all-asdf-files (&optional (quicklisp-controller-directory *quicklisp-controller-directory*))
  (setf *failed-asdf-files* nil)
  (format *standard-output* "Finding ASDF files...~%")
  (find-files-do
   (merge-pathnames #p"upstream-cache/" quicklisp-controller-directory)
   "*.asd"
   (lambda (file)
     ;; conflictive asdf system files
     (when (not (some (lambda (conflictive-system-name)
			(search conflictive-system-name (princ-to-string file) :test 'equalp))
		      *conflictive-asdf-files*))
       (format *standard-output* "Loading ~a" file)
       (handler-case (progn
		     (asdf/find-system:load-asd file)
		     (format *standard-output* ". Success.~%"))
       (error (e)
	 ;;(error e)
	 (push (cons file e) *failed-asdf-files*)
	 (format *standard-output* ". ERROR.~%")
	 ))))))

(defun collect-systems-documentation ()
  (mapcar (lambda (system)
	    (cons system (collect-system-documentation system)))
	  (asdf/system-registry:registered-systems*)))

(defun collect-system-documentation (system)
  (when (asdf:system-source-directory system)
    (let ((doc-files))
      (find-files-do (asdf:system-source-directory system)
		   "README*"
		   (lambda (file)
		     (push file doc-files))
		   nil)
      (find-files-do (asdf:system-source-directory system)
		   "doc/index.*"
		   (lambda (file)
		     (push file doc-files))
		   nil)
      (find-files-do (asdf:system-source-directory system)
		   "docs/index.*"
		   (lambda (file)
		     (push file doc-files))
		   nil)
      (find-files-do (asdf:system-source-directory system)
		     (format nil "doc/~a.*" (asdf:component-name system))
		     (lambda (file)
		       (push file doc-files))
		     nil)
      (find-files-do (asdf:system-source-directory system)
		     (format nil "docs/~a.*" (asdf:component-name system))
		     (lambda (file)
		       (push file doc-files))
		     nil)      
      doc-files)))

(defun process-systems-documentation (directory-path)
  (ensure-directories-exist directory-path)
  (dolist (system (asdf/system-registry:registered-systems*))
    (ensure-directories-exist (merge-pathnames (uiop/pathname:ensure-directory-pathname (asdf:component-name system)) directory-path)
