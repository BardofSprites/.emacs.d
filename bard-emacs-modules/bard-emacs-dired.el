(use-package wdired
  :ensure t)

(use-package dired
  :bind*
  (("C-j" . dired-jump))
  :config
  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "nsxiv" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
          (".gif" "mpv --loop=inf")
          (".*" "xdg-open")))
  :hook
  ((dired-mode . dired-hide-details-mode)
   ;; attachments for email through dired
   (dired-mode . turn-on-gnus-dired-mode)))

(setq dired-dwim-target t)
