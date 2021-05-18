;;;; webinfo.asd

(asdf:defsystem #:webinfo
  :description "Web and desktop Texinfo reader"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "mimeparse")
               (:file "webinfo")
               (:file "xmldoc")
               (:file "search")
               (:file "settings")
               (:file "sexp-node")
               (:file "winfo")
               (:file "user")
	       (:file "dir"))
  :depends-on (:anaphora
               :hunchentoot
               :cl-who
               :puri
               :drakma
               :cxml
               :xpath
               :trivia
	       :groupby
               :access
               :cl-forms
               :cl-forms.who
               :metabang-bind
               :assoc-utils
               :quri
               :montezuma
               :cl-intbytes
	       :string-case
	       :cl-tokyo-cabinet))
