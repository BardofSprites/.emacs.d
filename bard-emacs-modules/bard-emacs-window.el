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
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))
          ;; bottom side window
          ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                  (derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . backtrace-mode)))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t)))
          ("\\magit: .*"
	       (display-buffer-same-window)
	       (inhibit-same-window . nil)
	       (dedicated . t))
	      ("\\*Org Agenda\\*"
	       (display-buffer-same-window)
	       (inhibit-same-window . nil)
	       (dedicated . t))
          ("\\*cfw-calendar\\*"
	       (display-buffer-same-window)
	       (inhibit-same-window . nil)
	       (dedicated . t))
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom))
          ;; below current window
          ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.1)
           (dedicated . t)
           (preserve-size . (t . t)))
          ((derived-mode . reb-mode) ; M-x re-builder
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 4) ; note this is literal lines, not relative
           (dedicated . t)
           (preserve-size . (t . t)))
          ((or . ((derived-mode . occur-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . Buffer-menu-mode)
                  (derived-mode . log-view-mode)
                  (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                  "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
                  prot-window-shell-or-term-p
                  ;; ,world-clock-buffer-name
                  ))
           (prot-window-display-buffer-below-or-pop)
           (body-function . prot-window-select-fit-size))
          ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (dedicated . t)
           (window-height . fit-window-to-buffer))
          ("\\*ispell-top-choices\\*.*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ))
  )

(use-package frame
  :ensure nil
  :bind (("C-x u" . undelete-frame)
         ("C-x f" . other-frame-prefix)) ; I use only C-/ for `undo'
  :hook (after-init . undelete-frame-mode))

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

(use-package emacs
  :ensure nil
  :bind
  ("C-x w t" . tear-off-window)
  ("C-x w c" . clone-indirect-buffer-other-window))

(use-package ibuffer
  :ensure nil
  :config
  (setq ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-auto-mode t))

(use-package emacs
  :ensure nil
  :bind
  ("C-x w w" . bard/toggle-window-split))

(provide 'bard-emacs-window)
;;; bard-emacs-window.el ends here
