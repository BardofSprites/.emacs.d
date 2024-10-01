(require 'bard-window)

(use-package emacs
  ;; configuration for window splits/window sizes
  :config
  (setq focus-follows-mouse t)
  (setq mouse-autoselect-window t)
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 80)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30))

;; (use-package beframe
;;   :ensure t
;;   )

(use-package windmove
  :bind*
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right)
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left)))

(use-package emacs
  :config
  (setq display-buffer-alist
        `(("\\`\\*Async Shell Command\\*\\'"
	       (display-buffer-no-window))
	      ("\\`\\*\\(Warnings\\|Compile-Log\\|tex-shell\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t))
	      ("\\*\\(Calendar\\|wclock\\).*"
	       (display-buffer-reuse-mode-window display-buffer-below-selected)
	       (dedicated . t)
	       (window-height . fit-window-to-buffer))
	      ("\\magit: .*"
	       (display-buffer-same-window)
	       (inhibit-same-window . nil)
	       (dedicated . t))
	      ("\\*Org Agenda\\*"
	       (display-buffer-same-window)
	       (inhibit-same-window . nil)
	       (dedicated . t))
	      ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
	      ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
	       (display-buffer-reuse-mode-window display-buffer-below-selected))
	      ;; error stuff
	      ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                  (derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . backtrace-mode)
		          (derived-mode . cider-stacktrace-mode)))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t)))

	      ((or . ((derived-mode . occur-mode)
                  (derived-mode . grep-mode)
		          (derived-mode . Man-mode)
                  (derived-mode . Buffer-menu-mode)
                  (derived-mode . log-view-mode)
                  (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                  "\\*\\(|Buffer List\\|Occur\\|Man.*\\|Org Select\\|vc-change-log\\|eldoc.*\\).*"
                  prot-window-shell-or-term-p
                  ,world-clock-buffer-name))
           (prot-window-display-buffer-below-or-pop)
           (body-function . prot-window-select-fit-size))
	      ))
  )

(use-package winner-mode
  :init
  (winner-mode 1)
  :bind
  (("C-x <right>" . winner-redo)
   ("C-x <left>" . winner-undo)
   ("C-x C-n" . next-buffer)
   ("C-x C-p" . previous-buffer)
   ("C-x <up>" . next-buffer)
   ("C-x <down>" . previous-buffer)))

(provide 'bard-emacs-window)
;;; bard-emacs-window.el ends here
