(require 'bard-theme)

;; declare all themes as safe (i trust developers)
(setq custom-safe-themes t)

(custom-set-faces
      '(region ((t :extend nil))))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs nil
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold))

  ;; custom colors for certain parts of the modus vivendi
  (setq modus-vivendi-palette-overrides
        '((bg-region bg-dim)
          (fg-region yellow-faint)
          (fg-space bg-dim)))


  ;; custom color inherited by all modus themes
  (setq modus-themes-common-palette-overrides
        '((fg-space bg-dim)
          (comment fg-dim)))

  (setq modus-vivendi-deuteranopia-palette-overrides
        '((comment yellow-faint)))

  (setq modus-operandi-deuteranopia-palette-overrides
        '((comment yellow-faint)))

  (bard/select-theme 'modus-vivendi))

(use-package ef-themes
  :ensure t
  :defer t)

(use-package doric-themes
  :ensure t)

(use-package standard-themes
  :ensure t
  :defer t)

(use-package fontaine
  :ensure t
  :config
  ;; save file
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

(use-package rainbow-mode
  :ensure t)

(global-set-key (kbd "<f6>") #'bard/select-theme)

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode))

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 10
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 1
           :left-fringe-width 0
           :right-fringe-width 0
           :scroll-bar-width 0))
  (spacious-padding-mode t))

(provide 'bard-emacs-theme)
;;; bard-emacs-theme.el ends here
