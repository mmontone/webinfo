(require 'browse-url)
(require 'eww)

(defgroup webinfo ()
  "WebInfo customizations"
  :prefix "webinfo-")

(defcustom webinfo-url "http://localhost:9090"
  "The url of the WebInfo service"
  :type 'string
  :group 'webinfo)

(defcustom webinfo-browser 'eww
  "WebInfo browsing method"
  :type 'symbol
  :group 'webinfo)

(defun webinfo-apropos-symbol (symbol)
  (interactive "sSymbol:")
  (webinfo-browse-url (format "%s/_s?q=%s" webinfo-url symbol)))

(defun webinfo-lookup-symbol (symbol)
  (interactive "sSymbol:")
  (webinfo-browse-url (format "%s/_s?q=%s" webinfo-url symbol)))

;; TODO: look at eww-browse-with-external-browser

(defun webinfo-browse-url (url)
  (case webinfo-browser
    ('eww (eww-browse-url (concat url "&_c=eww")))
    ('browse-url (browse-url (concat url "&_c=browse-url")))
    (t (eww-browse-url url))))

(defvar webinfo-mode-map
  (let ((map (make-keymap)))
    (define-key map "t" 'webinfo-top-node)
    (define-key map "u" 'webinfo-up)
    (define-key map "n" 'webinfo-next)
    (define-key map "p" 'webinfo-prev)
    (define-key map "h" 'webinfo-help)
    (define-key map "s" 'webinfo-search)
    (define-key map "l" 'webinfo-history-back)
    (define-key map "f" 'webinfo-history-forward)
    (define-key map "g" 'webinfo-goto-node)
    (define-key map "a" 'webinfo-home-node)
    (define-key map "q" 'webinfo-exit)
    map))

(define-minor-mode webinfo-mode
  "WebInfo minor mode."
  :init-value nil
  :lighter " WebInfo"
  :keymap webinfo-mode-map
  :group 'webinfo)

(defun webinfo-history-back ()
  (interactive)
  (debug "history back"))

(defun webinfo-history-forward ()
  (interactive)
  (debug "webinfo history forward"))

(defun webinfo-prev ()
  (interactive)
  ;; add the _n parameter to the url
  ;; TODO: implement properly using some uri library
  (eww-browse-url (format "%s?_n=prev" (eww-current-url))))

(defun webinfo-next ()
  (interactive)
  ;; add the _n parameter to the url
  ;; TODO: implement properly using some uri library
  (eww-browse-url (format "%s?_n=next" (eww-current-url))))

(defun webinfo-up ()
  (interactive)
  ;; add the _n parameter to the url
  ;; TODO: implement properly using some uri library
  (eww-browse-url (format "%s?_n=up" (eww-current-url))))

(defun webinfo-top-node ()
  (interactive)
  (debug "webinfo-top"))

(defun webinfo-home-node ()
  (interactive)
  (debug "webinfo-home"))

(defun webinfo-goto-node ()
  (interactive)
  (debug "webinfo goto node"))

(defun webinfo-exit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun webinfo-index ()
  (interactive)
  (debug "webinfo index"))

(easy-menu-define
 webinfo-mode-menu webinfo-mode-map
 "Menu for webinfo files."
 '("webinfo"
   ["Up" webinfo-up :active (webinfo-check-pointer "up")
    :help "Go up in the webinfo tree"]
   ["Next" webinfo-next :active (webinfo-check-pointer "next")
    :help "Go to the next node"]
   ["Previous" webinfo-prev :active (webinfo-check-pointer "prev[ious]*")
    :help "Go to the previous node"]
   ["Backward" webinfo-backward-node
    :help "Go backward one node, considering all as a sequence"]
   ["Forward" webinfo-forward-node
    :help "Go forward one node, considering all as a sequence"]
   ["Beginning" beginning-of-buffer
    :help "Go to beginning of this node"]
   ["Top" webinfo-top-node
    :help "Go to top node of file"]
   ["Final Node" webinfo-final-node
    :help "Go to final node in this file"]
   ("Menu Item" ["You should never see this" report-emacs-bug t])
   ("Reference" ["You should never see this" report-emacs-bug t])
   ["Search..." webinfo-search
    :help "Search for regular expression in this webinfo file"]
   ["Search Next" webinfo-search-next
    :help "Search for another occurrence of regular expression"]
   ["Go to Node..." webinfo-goto-node
    :help "Go to a named node"]
   ["Back in history" webinfo-history-back :active webinfo-history
    :help "Go back in history to the last node you were at"]
   ["Forward in history" webinfo-history-forward :active webinfo-history-forward
    :help "Go forward in history"]
   ["History" webinfo-history :active webinfo-history-list
    :help "Go to menu of visited nodes"]
   ["Table of Contents" webinfo-toc
    :help "Go to table of contents"]
   ("Index"
    ["Lookup a String..." webinfo-index
     :help "Look for a string in the index items"]
    ["Next Matching Item" webinfo-index-next :active webinfo-index-alternatives
     :help "Look for another occurrence of previous item"]
    ["Lookup a string and display index of results..." webinfo-virtual-index
     :help "Look for a string in the index items and display node with results"]
    ["Lookup a string in all indices..." info-apropos
     :help "Look for a string in the indices of all manuals"])
   ["Copy Node Name" webinfo-copy-current-node-name
    :help "Copy the name of the current node into the kill ring"]
   ["Clone webinfo buffer" clone-buffer
    :help "Create a twin copy of the current webinfo buffer."]
   ["Exit" webinfo-exit :help "Stop reading webinfo"]))


(defvar webinfo-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu 'webinfo-history-back "left-arrow" map webinfo-mode-map
				   :rtl "right-arrow"
				   :label "Back"
				   :vert-only t)
    (tool-bar-local-item-from-menu 'webinfo-history-forward "right-arrow" map webinfo-mode-map
				   :rtl "right-arrow"
				   :label "Forward"
				   :vert-only t)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'webinfo-prev "prev-node" map webinfo-mode-map
				   :rtl "prev-node")
    (tool-bar-local-item-from-menu 'webinfo-next "next-node" map webinfo-mode-map
				   :rtl "next-node")
    
    (tool-bar-local-item-from-menu 'webinfo-up "up-node" map webinfo-mode-map
				   :vert-only t)
    
    (define-key-after map [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu 'webinfo-top-node "home" map webinfo-mode-map
				   :vert-only t)
    (tool-bar-local-item-from-menu 'webinfo-goto-node "jump-to" map webinfo-mode-map)
    
    (define-key-after map [separator-3] menu-bar-separator)
    (tool-bar-local-item-from-menu 'webinfo-index "index" map webinfo-mode-map
				   :label "Index")
    (tool-bar-local-item-from-menu 'webinfo-search "search" map webinfo-mode-map
				   :vert-only t)
    (tool-bar-local-item-from-menu 'webinfo-exit "exit" map webinfo-mode-map
				   :vert-only t)
    map))

(add-hook 'webinfo-mode-hook
          (lambda ()
            (setq-local tool-bar-map webinfo-tool-bar-map)))

(defun webinfo--detect-webinfo ()
  "Returns T when webinfo is detected at current url."
  ;; TODO. Perhaps use (eww-parse-headers)
  nil)

(add-hook 'eww-after-render-hook
          (lambda ()
            (when (webinfo--detect-webinfo)
              (webinfo-mode))))
