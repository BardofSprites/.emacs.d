(require 'bard-theme)

;; declare all themes as safe (i trust developers)
(setq custom-safe-themes t)

;; Ef-themes
(use-package ef-themes
  :ensure t
  :demand t
  :hook
  ((ef-themes-post-load . bard/ef-themes-hl-todo-faces)
   (ef-themes-post-load . logos-update-fringe-in-buffers))
  :config
  (defun bard/ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("WAIT" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("THEM" . ,magenta)
              ("PROG" . ,cyan-warmer)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("KLUDGE" . ,cyan)
              ("HACK" . ,cyan)
              ("TEMP" . ,red)
              ("FIXME" . ,red-warmer)
              ("XXX+" . ,red-warmer)
              ("KILLED" . ,cyan)
              ("REVIEW" . ,red)
              ("DEPRECATED" . ,yellow)))))

  (setq org-priority-faces
        '((?A . (:inherit (bold next-error)))
          (?B . org-priority)
          (?C . (:inherit (shadow org-priority)))))

  (setq ef-themes-headings
        '((agenda-date . (variable-pitch regular 1.1))
          (agenda-structure . (variable-pitch light 1.3))
          (t . 1.1)))

  ;; better faces in overrides
  (setq ef-themes-common-palette-overrides
        `((fringe unspecified)
          (bg-line-number-active bg-hl-line)
          (bg-line-number-inactive unspecified)
          (fg-line-number-active fg-main)
          (bg-tab-bar bg-dim)))

  ;; verbatim need higher contrast for org mode
  (setq ef-elea-dark-palette-overrides
        '((prose-verbatim yellow-cooler)))

  (setq ef-elea-light-palette-overrides
        '((prose-verbatim yellow-cooler)))

  (define-key global-map (kbd "M-<f5>") #'ef-themes-select)
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t)

  )

(use-package modus-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . modus-themes-select))
  :hook ((modus-themes-post-load . bard/modus-themes-hl-todo-faces)
         (modus-themes-post-load . logos-update-fringe-in-buffers))
  :config
  (defun bard/modus-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Modus themes colors.
The exact color values are taken from the active Modus theme."
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("WAIT" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("THEM" . ,magenta)
              ("PROG" . ,cyan-warmer)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("KLUDGE" . ,cyan)
              ("HACK" . ,cyan)
              ("TEMP" . ,red)
              ("FIXME" . ,red-warmer)
              ("XXX+" . ,red-warmer)
              ("REVIEW" . ,red)
              ("KILLED" . ,cyan)
              ("DEPRECATED" . ,yellow)))))

  (setq modus-themes-headings
        '((agenda-date . (variable-pitch regular 1.1))
          (agenda-structure . (variable-pitch light 1.3))
          (t . 1.1)))

  (setq modus-themes-custom-auto-reload nil
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold))
  (setq modus-themes-common-palette-overrides
        `((fringe unspecified)
          (bg-line-number-active bg-hl-line)
          (bg-line-number-inactive unspecified)
          (fg-line-number-active fg-main)
          (bg-tab-bar bg-dim)))
  ;; org customization
  (setq org-priority-faces
        '((?A . (:inherit (bold next-error)))
          (?B . org-priority)
          (?C . (:inherit (shadow org-priority)))))

  ;; (load-theme 'modus-vivendi t)
  )

(use-package gruber-darker-theme
  :ensure t
  :config
  (bard/select-theme 'gruber-darker))

;;;; Fonts
(use-package fontaine
  :ensure t
  :config
  ;; save file
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'laptop))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; preserve fonts when switching themes
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

(use-package rainbow-mode
  :ensure t)

(global-set-key (kbd "<f6>") #'bard/select-theme)

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
