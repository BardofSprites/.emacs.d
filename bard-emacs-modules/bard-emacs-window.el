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
  (setq beframe-create-frame-scratch-buffer nil)
  (setq beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
  (beframe-mode 1)

  (define-key global-map (kbd "C-x f") #'other-frame-prefix)
  (define-key global-map (kbd "C-c b") beframe-prefix-map)
  (define-key global-map (kbd "C-x C-b") #'beframe-buffer-menu)
  (define-key global-map (kbd "C-x B") #'select-frame-by-name)

  ;; Consult integration
  (defvar consult-buffer-sources)
     (declare-function consult--buffer-state "consult")

     (with-eval-after-load 'consult
       (defface beframe-buffer
         '((t :inherit font-lock-string-face))
         "Face for `consult' framed buffers.")

       (defun my-beframe-buffer-names-sorted (&optional frame)
         "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
     With optional argument FRAME, return the list of buffers of FRAME."
         (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

       (defvar beframe-consult-source
         `( :name     "Frame-specific buffers (current frame)"
            :narrow   ?F
            :category buffer
            :face     beframe-buffer
            :history  beframe-history
            :items    ,#'my-beframe-buffer-names-sorted
            :action   ,#'switch-to-buffer
            :state    ,#'consult--buffer-state))

       (add-to-list 'consult-buffer-sources 'beframe-consult-source)))

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
(define-key global-map (kbd "C-x <right>") #'winner-redo)
(define-key global-map (kbd "C-x <left>") #'winner-undo)
(define-key global-map (kbd "C-x C-n") #'next-buffer)
(define-key global-map (kbd "C-x C-p") #'previous-buffer)
(define-key global-map (kbd "C-x <up>") #'next-buffer)
(define-key global-map (kbd "C-x <down>") #'previous-buffer)

(provide 'bard-emacs-window)
;;; bard-emacs-window.el ends here
