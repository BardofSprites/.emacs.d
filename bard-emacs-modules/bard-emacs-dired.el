(define-key global-map (kbd "C-j") #'dired-jump)
(with-eval-after-load "org-mode"
  (define-key org-mode-map (kbd "C-j") nil))

  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
          (".*" "xdg-open")))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
