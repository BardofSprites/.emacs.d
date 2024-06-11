(use-package dired-subtree
  :ensure t
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package wdired
  :ensure t)

(use-package dired
  :bind*
  (("C-j" . dired-jump))
  :bind (:map dired-mode-map
              (("E" . emms-add-dired)
               ("<tab>" . dired-subtree-toggle)
               ("<backtab>" . dired-subtree-cycle)))
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

;; Image dired
(use-package image-dired
  :bind
  (:map dired-mode-map
        ((")" . image-dired-dired-display-external)))
  :config
  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "nsxiv")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4))

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
