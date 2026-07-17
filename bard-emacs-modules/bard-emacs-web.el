;;; IRC
(use-package circe
  :ensure t
  :defer t
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
           ;; :channels ("#emacs" "##anime" "#gentoo")
           :nickserv-password my-nickserv-password))))

(use-package elfeed-protocol
  :ensure t
  :after elfeed
  :config
  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-protocol-enable))

(use-package elfeed
  :ensure t
  :config
  (require 'bard-web)
  (setq elfeed-search-filter "+unread")

  (setq elfeed-feeds '(("fever+https://bard@feeds.bardman.dev/fever/"
                        :api-url "https://feeds.bardman.dev/fever/"
                        :use-authinfo t)))

  :bind
  ("C-c r" . elfeed)
  (:map elfeed-search-mode-map
        ;; C-p for play now
        ("C-c C-p" . bard/play-elfeed-video)
        ;; C-e for EMMS
        ("C-c C-e" . bard/add-video-emms-queue)
        ;; C-w for watch later
        ("C-c C-w" . bard/add-video-watch-later)
        ;; F is for fetch
        ("F"       . elfeed-update)))

(use-package eww
  :defer t
  :config
  (setq browse-url-handlers
        '(("wikipedia\\.org" . eww-browse-url)
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

  (defun skip-empty-li (shrtagli dom)
    (when (cddr dom)
      (funcall shrtagli dom)))

  ;; remove empty <li> tags, specifically for wiktionary
  ;; https://nickdrozd.github.io/2022/12/19/wiktionary-advice.html
  (advice-add
   'shr-tag-li :around
   #'skip-empty-li)

  ;; eww
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)")
  :bind
  ("C-c w" . eww)
  :hook
  (eww-mode . logos-focus-mode))

(provide 'bard-emacs-web)
;;; bard-emacs-web.el ends here
