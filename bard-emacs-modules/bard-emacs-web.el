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
