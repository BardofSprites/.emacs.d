(use-package dired-subtree
  :ensure t
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package wdired
  :ensure t)

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

;; Taken from https://superuser.com/a/176629
(defun bard/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))


;; (define-key image-dired-thumbnail-mode-map
;; 	    (kbd "<return>") #'image-dired-thumbnail-display-external)
