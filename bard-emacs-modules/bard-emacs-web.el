;;; IRC
(use-package circe
  :ensure t
  :config
  (setq auth-sources '("~/.authinfo.gpg"))

  (defun my-fetch-password (&rest params)
    (require 'auth-source)
    (let ((match (car (apply 'auth-source-search params))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        (error "Password not found for %S" params))))

  (defun my-nickserv-password (server)
    (my-fetch-password :user "bardman" :machine "irc.libera.chat"))

  (setq circe-network-options
        '(("Libera Chat"
           :nick "bardman"
           :channels ("#emacs" "##anime" "#gentoo")
           :nickserv-password my-nickserv-password))))

;;; RSS Feeds
(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-c e") 'elfeed)
  (setq elfeed-search-filter "+unread -academia"))

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/feeds.org"
				   "~/.emacs.d/youtube.org")))

(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/powerline-default-separator 'box))

;;; Web Browsing (EWW and Firefox/Librewolf)

(use-package eww
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser)

  (setq browse-url-handlers
        '(("wikipedia\\.org" . eww-browse-url)
          ;; ("github" . browse-url-chromium)
          ("github" . browse-url-default-browser)
          ("youtube.com" . browse-url-default-browser)
          ("reddit.com" . browse-url-default-browser)))

  ;; shr optimizations
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)
  (setq shr-width fill-column)
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil)

  ;; eww
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)")
  :bind
  ("C-c w" . eww))

;; librewolf open browser

;; TODO fix this to work with librewolf-bin on Gentoo
(defun browse-url-librewolf (url &optional new-window)
  "Ask the Librewolf WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-librewolf-arguments' to Librewolf.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Librewolf window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-librewolf-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "librewolf " url) nil
           "librewolf"
            (list url))))

(provide 'bard-emacs-web)
;;; bard-emacs-web.el ends here
