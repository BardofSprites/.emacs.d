;; Generic variables for modes
(use-package whitespace
  :ensure nil
  :demand t
  :config
  (setq whitespace-style '(face
                           tabs
                           spaces
                           trailing
                           space-before-tab
                           newline indentation
                           empty space-after-tab
                           space-mark tab-mark))
  :hook
  (prog-mode . whitespace-mode))

(use-package display-line-numbers
  :ensure nil
  :demand t
  :bind
  (("<f12>" . display-line-numbers-mode))
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; padding for everything
(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 20
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 20
           :left-fringe-width 0
           :right-fringe-width 0
           :scroll-bar-width 0))
  (spacious-padding-mode t))

;; Todo Keywords highlighting, colors defined in bard-emacs-theme.el
(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(provide 'bard-emacs-ui)

;;; bard-emacs-ui.el ends here
