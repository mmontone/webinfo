(require 'browse-url)

(defvar webinfo-url "http://localhost:9090")
(defvar webinfo-browser 'eww)

(defun webinfo-apropos-symbol (symbol)
  (interactive "sSymbol:")
  (webinfo-browse-url (format "%s/_s?q=%s" webinfo-url symbol)))

;; TODO: look at eww-browse-with-external-browser

(defun webinfo-browse-url (url)
  (case webinfo-browser
    ('eww (eww-browse-url (concat url "&_c=eww")))
    ('browse-url (browse-url (concat url "&_c=browse-url")))
    (t (eww-browse-url url))))
