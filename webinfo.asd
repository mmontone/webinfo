;;;; webinfo.asd

(asdf:defsystem #:webinfo
  :description "Web and desktop TexInfo reader"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "webinfo"))
  :depends-on (:anaphora
               :hunchentoot
               :cl-who
               :puri
               :drakma
               :cxml
               :xpath
               :trivia))
