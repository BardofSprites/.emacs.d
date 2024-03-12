(define-key global-map (kbd "C-j") #'dired-jump)
(add-hook 'org-mode-hook
          (lambda()
            (local-unset-key (kbd "C-j"))))

  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
          (".*" "xdg-open")))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
