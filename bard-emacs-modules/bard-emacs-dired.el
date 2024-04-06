(use-package dired-subtree
  :ensure t
  :config
  (setq dired-subtree-use-backgrounds nil))

(define-key global-map (kbd "C-j") #'dired-jump)
(add-hook 'org-mode-hook
          (lambda()
            (local-unset-key (kbd "C-j"))))

(setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
      '(("\\.\\(png\\|jpe?g\\|tiff\\)" "nsxiv" "feh" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
	(".gif" "mpv --loop=inf")
        (".*" "xdg-open")))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(add-hook 'dired-mode-hook
          (lambda()
            (define-key dired-mode-map (kbd "E") #'emms-add-dired)
	    (define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
	    (define-key dired-mode-map (kbd "<backtab>") #'dired-subtree-cycle)))

;; hook for attaching emails mode
(add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

(setq dired-dwim-target t)

;; Image dired
(setq image-dired-thumbnail-storage 'standard)
(setq image-dired-external-viewer "nsxiv")
(setq image-dired-thumb-size 80)
(setq image-dired-thumb-margin 2)
(setq image-dired-thumb-relief 0)
(setq image-dired-thumbs-per-row 4)


;; (define-key image-dired-thumbnail-mode-map
;; 	    (kbd "<return>") #'image-dired-thumbnail-display-external)
