(require 'bard-window)

(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)
(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq split-height-threshold 80)
(setq split-width-threshold 125)
(setq window-min-height 3)
(setq window-min-width 30)

(define-key global-map (kbd "C-x f") #'other-frame-prefix)

(use-package eyebrowse
  :ensure t
  :bind*
  (("M-0" . eyebrowse-switch-to-window-config-0)
   ("M-1" . eyebrowse-switch-to-window-config-1)
   ("M-2" . eyebrowse-switch-to-window-config-2)
   ("M-3" . eyebrowse-switch-to-window-config-3)
   ("M-4" . eyebrowse-switch-to-window-config-4)
   ("M-5" . eyebrowse-switch-to-window-config-5)
   ("M-6" . eyebrowse-switch-to-window-config-6)
   ("M-7" . eyebrowse-switch-to-window-config-7)
   ("M-8" . eyebrowse-switch-to-window-config-8)
   ("M-9" . eyebrowse-switch-to-window-config-9))
  )

(use-package emacs
  :bind*
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right)
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left)))

(setq display-buffer-alist
      `(("\\`\\*Async Shell Command\\*\\'"
	 (display-buffer-no-window))
	("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
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

(winner-mode 1)
(let ((map global-map))
     (define-key map (kbd "C-x <right>") #'winner-redo)
     (define-key map (kbd "C-x <left>") #'winner-undo)
     (define-key map (kbd "C-x C-n") #'next-buffer)
     (define-key map (kbd "C-x C-p") #'previous-buffer)
     (define-key map (kbd "C-x <up>") #'next-buffer)
     (define-key map (kbd "C-x <down>") #'previous-buffer))

(define-key global-map (kbd "C-x w") #'delete-frame)

(provide 'bard-emacs-window)
;;; bard-emacs-window.el ends here
