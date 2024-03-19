(require 'bard-window)

;; settings for prot-
(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq split-height-threshold 80)
(setq split-width-threshold 125)
(setq window-min-height 3)
(setq window-min-width 30)

(define-key global-map (kbd "C-M-<up>") #'windmove-up)
(define-key global-map (kbd "C-M-<right>") #'windmove-right)
(define-key global-map (kbd "C-M-<down>") #'windmove-down)
(define-key global-map (kbd "C-M-<left>") #'windmove-left)
(define-key global-map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
(define-key global-map (kbd "C-M-S-<right>") #'windmove-swap-states-right)
(define-key global-map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
(define-key global-map (kbd "C-M-S-<left>") #'windmove-swap-states-left)

(use-package beframe
  :ensure t
  :config
  (setq beframe-functions-in-frames '(project-prompt-project-dir))

  (beframe-mode 1)

  (define-key global-map (kbd "C-x f") #'other-frame-prefix)
  (define-key global-map (kbd "C-c b") beframe-prefix-map)
  (define-key global-map (kbd "C-x C-b") #'beframe-buffer-menu)
  (define-key global-map (kbd "C-x B") #'select-frame-by-name))

(setq display-buffer-alist
      `(("\\`\\*Async Shell Command\\*\\'"
	 (display-buffer-no-window))
	("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	 (display-buffer-no-window)
	 (allow-no-window . t))
	("\\*\\(Calendar\\).*"
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
                (derived-mode . Buffer-menu-mode)
                (derived-mode . log-view-mode)
                (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
                prot-window-shell-or-term-p
                ,world-clock-buffer-name))
         (prot-window-display-buffer-below-or-pop)
         (body-function . prot-window-select-fit-size))
	))

(winner-mode 1)
(define-key global-map (kbd "C-x <right>") #'winner-redo)
(define-key global-map (kbd "C-x <left>") #'winner-undo)
(define-key global-map (kbd "C-x C-n") #'next-buffer)
(define-key global-map (kbd "C-x C-p") #'previous-buffer)
(define-key global-map (kbd "C-x <up>") #'next-buffer)
(define-key global-map (kbd "C-x <down>") #'previous-buffer)
