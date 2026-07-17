(setq fontaine-presets
      '((default
         :default-height 140
         :default-family "Iosevka Comfy"
         :variable-pitch-family "Iosevka Comfy"
         :variable-pitch-height 1.0
         :fixed-pitch-family "Iosevka Comfy"
         :fixed-pitch-height 1.0
         :bold-weight bold
         )
        (tiny
         :inherit default
         :default-height 135)
        (wide
         :default-height 135
         :default-family "Iosevka Comfy Wide"
         :fixed-pitch-family "Iosevka Comfy Wide"
         :variable-pitch-family "Iosevka Comfy Wide Motion Duo")
        (prot
         :default-family "Iosevka Comfy Wide Motion"
         :default-height 140
         :default-weight medium
         :fixed-pitch-family "Iosevka Comfy Wide Motion"
         :variable-pitch-family "Iosevka Comfy Wide Motion Duo"
         :bold-weight extrabold)
        (duo
         :default-family "Iosevka Comfy Motion"
         :default-height 140
         :default-weight medium
         :fixed-pitch-family "Iosevka Comfy Motion"
         :variable-pitch-family "Iosevka Comfy Motion Duo"
         :bold-weight extrabold)
        (mono
         :default-height 140
         :default-family "Terminus"
         :fixed-pitch-family "Terminus"
         :variable-pitch-family "Terminus")
        (mono-large
         :inherit mono
         :default-height 150
         :variable-pitch-height 160)
        (mac
         :default-height 130
         :default-family "Monaco"
         :variable-pitch-family "Monaco"
         :fixed-pitch-family "Monaco")
        (large
         :inherit default
         :default-height 160)
        (huge
         :inherit default
         :default-height 180)
        (t
         :default-family "Monospace")))

(set-fontset-font
 t 'han
 (font-spec :family "Noto Serif CJK JP") nil 'prepend)

(defun bard/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defvar bard/after-theme-load-hook nil
  "Hook that runs after a new theme is loaded using `bard/select-theme`.")

(dolist (hook '(bard/after-theme-load-hook))
  (add-hook hook #'logos-update-fringe-in-buffers))

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
