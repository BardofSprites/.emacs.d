(setq browse-url-browser-function 'eww-browse-url)
(setq browse-url-secondary-browser-function 'browse-url-default-browser)

(setq shr-use-colors nil)             ; t is bad for accessibility
(setq shr-use-fonts nil)              ; t is not for me
(setq shr-max-image-proportion 0.6)
(setq shr-image-animate nil)          ; No GIFs, thank you!
(setq shr-width fill-column)          ; check `prot-eww-readable'
(setq shr-max-width fill-column)
(setq shr-discard-aria-hidden t)
(setq shr-cookie-policy nil)

;; eww
(setq eww-search-prefix "https://duckduckgo.com/html/?q=")
(setq eww-history-limit 150)
(setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)")

(global-set-key (kbd "C-c w") 'eww)
