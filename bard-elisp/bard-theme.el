(setq fontaine-presets
        '((default
           :default-height 160
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion"
           :variable-pitch-height 1.0
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :bold-weight bold
           :mode-line-active-family "Iosevka Comfy Motion"
           :mode-line-inactive-family "Iosevka Comfy Motion"
           :mode-line-active-height 160
           :mode-line-inactive-height 160
           )
          (mixed
           :inherit default
           :variable-pitch-family "Iosevka Comfy Wide Motion Duo")
          (wide
           :inherit default
           :default-height 150
           :default-family "Iosevka Comfy Wide"
           :fixed-pitch-family "Iosevka Comfy Wide"
           :mode-line-active-family "Iosevka Comfy Wide Motion Duo"
           :mode-line-inactive-family "Iosevka Comfy Wide Motion Duo"
           :variable-pitch-family "Iosevka Comfy Wide Motion Duo"
           :mode-line-active-height 150
           :mode-line-inactive-height 150)
          (large
           :inherit default
           :variable-pitch-family "Iosevka Comfy Wide Motion"
           :default-height 170
           :mode-line-active-height 150
           :mode-line-inactive-height 150
           )
          (huge
           :inherit default
           :variable-pitch-family "Iosevka Comfy Wide Motion"
           :default-height 200
           :mode-line-active-height 170
           :mode-line-inactive-height 170
           )
          (comic
           :inherit default
           :default-family "Comic Code"
           :variable-pitch-family "Comic Code"
           :fixed-pitch-family "Comic Code"
           :mode-line-active-family "Comic Code"
           :mode-line-inactive-family "Comic Code"
           )
          (writing
           :inherit default
           :variable-pitch-family "Lora")
          (bit
           :inherit default
           :default-height 180
           :mode-line-active-height 180
           :mode-line-inactive-height 180
           :default-family "Terminus"
           :variable-pitch-family "Terminus"
           :fixed-pitch-family "Terminus"
           :mode-line-active-family "Terminus"
           :mode-line-inactive-family "Terminus"
           )
          (t
           :default-family "Monospace"
           )))

;; Switching themes
(defun bard/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defvar bard/after-theme-load-hook nil
  "Hook that runs after a new theme is loaded using `bard/select-theme`.")

(dolist (hook '(enable-theme-hook))
  (add-hook hook #'fontaine-apply-current-preset)
  (add-hook hook #'logos-update-fringe-in-buffers))

(add-to-list 'enable-theme-functions #'logos-update-fringe-in-buffers)

(add-to-list 'enable-theme-functions #'fontaine-apply-current-preset)

(defun bard/select-theme ()
  "Prompt the user to select a theme from all available custom themes and enable it."
  (interactive)
  (let* ((theme (completing-read "Select theme: " (mapcar 'symbol-name (custom-available-themes))))
         (theme-symbol (intern theme))
         (theme-name (if (string-suffix-p "-theme" theme)
                         (substring theme 0 -6)
                       theme))
         (colored-theme-name (propertize theme-name 'face '(:weight bold))))
    (bard/disable-all-themes)
    (load-theme theme-symbol t)
    (message "Loaded the %s theme" colored-theme-name)
    (run-hooks 'after-theme-load-hook)))


(provide 'bard-theme)
