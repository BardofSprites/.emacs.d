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
         :default-height 130
         :default-weight medium
         :fixed-pitch-family "Iosevka Comfy Wide Motion"
         :variable-pitch-family "Iosevka Comfy Wide Duo"
         :bold-weight extrabold)
        (mono
         :default-height 130
         :default-family "monospace"
         :fixed-pitch-family "monospace"
         :variable-pitch-family "Baskerville"
         :variable-pitch-height 140)
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
  (add-hook hook #'fontaine-apply-current-preset)
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

(defun bard/outline-heading-faces ()
  (set-face-attribute 'org-document-title nil
                      :inherit '(outline-1 variable-pitch)
                      :weight 'light
                      :height 1.5)

  (set-face-attribute 'org-level-1 nil
                      :inherit '(outline-2 variable-pitch)
                      :weight 'light
                      :height 1.3)

  (set-face-attribute 'org-level-2 nil
                      :inherit 'outline-3
                      :height 1.2)

  (set-face-attribute 'org-level-3 nil
                      :inherit '(outline-4 variable-pitch)
                      :height 1.1)

  (set-face-attribute 'org-level-4 nil
                      :inherit '(outline-5 variable-pitch)
                      :height 1.1)

  (set-face-attribute 'org-level-5 nil
                      :inherit '(outline-6 variable-pitch)
                      :height 1.1)

  (set-face-attribute 'org-level-6 nil
                      :inherit '(outline-6 variable-pitch)
                      :height 1.1)

  (set-face-attribute 'org-agenda-date nil
                      :inherit 'variable-pitch
                      :weight 'bold
                      :height 1.3)

  (set-face-attribute 'org-agenda-structure nil
                      :inherit 'variable-pitch
                      :weight 'bold
                      :height 1.5))

(add-hook 'bard/after-theme-load-hook #'bard/outline-heading-faces)

(provide 'bard-theme)
