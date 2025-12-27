(use-package dired-subtree
  :ensure t
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-preview
  :ensure t
  :bind
  (:map dired-mode-map
        ("P" . dired-preview-mode))
  :config
  (setq dired-preview-delay 0.1))

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
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-AgGFhlv --group-directories-first --time-style=long-iso")
  :hook
  ((dired-mode . dired-hide-details-mode)
   ;; attachments for email through dired
   (dired-mode . turn-on-gnus-dired-mode)))

(use-package image-dired
  :bind
  (:map dired-mode-map
        ((")" . image-dired-dired-display-external)
         ("B" . bard/dired-set-background-with-feh)))
  :bind
  ("C-x C-d" . image-dired)
  :bind
  (:map image-dired-thumbnail-mode-map ("B" . bard/image-dired-set-background-with-feh))
  :config
  (define-advice image-dired-display-image (:override (file &optional _ignored))
    (setq file (expand-file-name file))
    (when (not (file-exists-p file))
      (error "No such file: %s" file))
    (let ((buf (get-buffer image-dired-display-image-buffer))
          (cur-win (selected-window)))
      (when buf
        (kill-buffer buf))
      (when-let ((buf (find-file-noselect file nil t)))
        (with-current-buffer buf
          (rename-buffer image-dired-display-image-buffer)
          (if (string-match (image-file-name-regexp) file)
              (image-dired-image-mode)
            ;; Support visiting PDF files.
            (normal-mode))
          (display-buffer buf))
        (select-window cur-win))))

  (setq image-dired-thumbnail-storage 'standard)
  (setq image-dired-external-viewer "nsxiv")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)

  (defun bard/dired-set-background-with-feh ()
    "Set the selected image as the background using feh."
    (interactive)
    (let ((image-file (dired-get-file-for-visit)))
      (start-process "feh" nil "feh" "--bg-fill" image-file)
      (message "Background set to %s" image-file)))

  (defun bard/image-dired-set-background-with-feh ()
    "Set the selected image as the background using feh."
    (interactive)
    (let ((image-file (image-dired-original-file-name)))
      (start-process "feh" nil "feh" "--bg-fill" image-file)
      (message "Background set to %s" image-file))))

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

(use-package dired-video-thumbnail
  :ensure t
  :vc (:url "https://github.com/captainflasmr/dired-video-thumbnail"
            :rev :newest)
  :bind (:map dired-mode-map
              ("C-t v" . dired-video-thumbnail)))

(provide 'bard-emacs-dired)
