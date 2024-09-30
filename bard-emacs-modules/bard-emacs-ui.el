;; Generic variables for modes
(use-package emacs
  :bind
  (("<f12>" . display-line-numbers-mode))
  :config
  (setq display-line-numbers-type 'relative)
  (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))

;; display line numbers custom mode
(define-minor-mode bard/display-line-numbers-mode
  "Toggle `display-line-numbers-mode' and  `hl-line-mode' in tandem"
  :init-value nil
  :global nil
  (if (bard/display-line-numbers-mode
       (progn
	 (display-line-numbers-mode 1)
	 (hl-line-mode 1))
       (display-line-numbers-mode -1)
       (hl-line-mode -1))))

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
  :init
  (global-hl-todo-mode t))

(provide 'bard-emacs-ui)

;;; bard-emacs-ui.el ends here
