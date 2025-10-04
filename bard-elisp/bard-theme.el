(setq fontaine-presets
        '((default
           :default-height 140
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion"
           :variable-pitch-height 1.0
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :bold-weight bold
           )
          (tiny
           :inherit default
           :default-height 135)
          (wide
           :inherit default
           :default-family "Iosevka Comfy Wide"
           :fixed-pitch-family "Iosevka Comfy Wide"
           :variable-pitch-family "Iosevka Comfy Wide Motion Duo")
          (large
           :inherit default
           :default-height 160
           )
          (huge
           :inherit default
           :default-height 180
           )
          (t
           :default-family "Monospace"
           )))

(set-fontset-font
 t 'han
 (font-spec :family "Noto Serif CJK JP") nil 'prepend)

;;; Switching themes
(defun bard/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defvar bard/after-theme-load-hook nil
  "Hook that runs after a new theme is loaded using `bard/select-theme`.")

(dolist (hook '(bard/after-theme-load-hook))
  (add-hook hook #'fontaine-apply-current-preset)
  (add-hook hook #'logos-update-fringe-in-buffers)
  (add-hook hook #'bard/update-ryo-cursor-color))

(defun bard/select-theme (&optional theme)
  "Enable the specified THEME, or prompt the user to select one if THEME is nil."
  (interactive
   (list
    (completing-read "Select theme: "
                     (mapcar 'symbol-name (custom-available-themes)))))
  (let* ((theme-symbol (if (symbolp theme) theme (intern theme)))
         (theme-name (symbol-name theme-symbol))
         (display-theme-name (if (string-suffix-p "-theme" theme-name)
                                 (substring theme-name 0 -6)
                               theme-name))
         (colored-theme-name (propertize display-theme-name 'face '(:weight bold))))
    (bard/disable-all-themes)
    (load-theme theme-symbol t)
    (message "Loaded the %s theme" colored-theme-name)
    (run-hooks 'bard/after-theme-load-hook)))

(defun bard/update-ryo-cursor-color ()
  "Update the color variable of `ryo-modal-mode' cursor to match the ef/modus theme."
  (let ((active-theme (car custom-enabled-themes))
        (cursor-color nil))
    (cond
     ((and (fboundp 'ef-themes-with-colors)
           (string-prefix-p "ef-" (symbol-name active-theme)))
      (ef-themes-with-colors
        (setq ryo-modal-cursor-color cursor
              ryo-modal-default-cursor-color cursor)))
     ((and (fboundp 'modus-themes-with-colors)
           (string-prefix-p "modus-" (symbol-name active-theme)))
      (modus-themes-with-colors
        (setq ryo-modal-cursor-color cursor
              ryo-modal-default-cursor-color cursor)))
     (t (setq cursor-color "red"))
     (setq ryo-modal-cursor-color cursor-color
           ryo-modal-default-cursor-color cursor-color))))

(defvar my-last-cursor-type nil)

(defun bard/update-cursor-type ()
  "Set cursor type to 'bar in text modes, 'box otherwise.
leave it alone in pdf-view-mode."
  (unless (derived-mode-p 'pdf-view-mode)
    (let ((new-cursor (if (derived-mode-p 'text-mode) 'bar 'box)))
      (unless (eq my-last-cursor-type new-cursor)
        (setq cursor-type new-cursor)
        (setq my-last-cursor-type new-cursor)))))

(add-hook 'post-command-hook #'bard/update-cursor-type)

(provide 'bard-theme)
