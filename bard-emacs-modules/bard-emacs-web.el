;;; IRC

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

(provide 'bard-emacs-web)
;;; bard-emacs-web.el ends here
