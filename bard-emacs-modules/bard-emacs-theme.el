(require 'bard-theme)

;; declare all themes as safe (i trust developers)
(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-gruvbox-dark-variant "hard")
  (bard/select-theme 'doom-gruvbox)
  )

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode t)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  )

;;;; Fonts
(use-package fontaine
  :ensure nil
  :config
  ;; save file
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; preserve fonts when switching themes
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

(use-package rainbow-mode
  :ensure t)

(global-set-key (kbd "M-<f6>") #'bard/select-theme)

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  )

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
