(defpackage :webinfo/utils
  (:use :cl)
  (:export :get-temporary-file-pathname))

(in-package :webinfo/utils)

(defun get-temporary-file-pathname (&key directory (type "tmp" typep) prefix (suffix (when typep "-tmp")))
  "The temporary file's pathname will be based on concatenating
PREFIX (or \"tmp\" if it's NIL), a random alphanumeric string,
and optional SUFFIX (defaults to \"-tmp\" if a type was provided)
and TYPE (defaults to \"tmp\", using a dot as separator if not NIL),
within DIRECTORY (defaulting to the TEMPORARY-DIRECTORY) if the PREFIX isn't absolute."
  (let* ((prefix-pn (uiop/stream::ensure-absolute-pathname
                     (or prefix "tmp")
                     (or (uiop/stream::ensure-pathname
                          directory
                          :namestring :native
                          :ensure-directory t
                          :ensure-physical t)
                         #'uiop/stream:temporary-directory)))
         (prefix-nns (uiop/stream::native-namestring prefix-pn))
         (counter (random (expt 36 #-gcl 8 #+gcl 5))))
    (uiop/stream::parse-native-namestring
     (format nil "~A~36R~@[~A~]~@[.~A~]"
             prefix-nns counter suffix (unless (eq type :unspecific) type)))))
