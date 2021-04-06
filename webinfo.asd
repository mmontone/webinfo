;;;; webinfo.asd

(asdf:defsystem #:webinfo
  :description "Web and desktop TexInfo reader"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "webinfo")
               (:file "xmldoc")
               (:file "search")
               (:file "sexp-node")
               (:file "lispdoc")
               (:file "sinfo")
               (:file "user"))
  :depends-on (:anaphora
               :hunchentoot
               :cl-who
               :puri
               :drakma
               :cxml
               :xpath
               :trivia
               :access
               :cl-forms
               :cl-forms.who
               :metabang-bind
               :assoc-utils
               :quri
               :montezuma
               :cl-intbytes))
