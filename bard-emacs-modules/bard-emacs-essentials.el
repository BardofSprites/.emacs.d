(require 'bard-search)

;; writeable grep buffers
(use-package wgrep
  :ensure t
  :bind
  (:map wgrep-mode-map
	    ("C-x C-s" . wgrep-save-all-buffers)
        ("e" . wgrep-change-to-wgrep-mode)
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit)))

;; preview replace
(use-package iedit
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package substitute
  :ensure t
  :bind
  (("C-c s b" . substitute-target-below-point)
   ("C-c s a" . substitute-target-above-point)
   ("C-c s d" . substitute-target-in-defun)
   ("C-c s s" . substitute-target-in-buffer)))

;;; General Keybinds

;; Buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Time Management
;; Modeline
(setq display-time-format "%Y-%m-%d (%A) %H:%M")
(setq display-time-interval 60)
(setq display-time-default-load-average nil)
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)
(setq display-time-string-forms
      '((propertize
         (format-time-string display-time-format now)
         'face 'display-time-date-and-time
         'help-echo (format-time-string "%a %b %e, %Y" now))
        " "))
(display-time-mode 1)

;; running emacs as server
(require 'server)
(setq server-client-instructions nil)
(unless (server-running-p)
  (server-start))

(provide 'bard-emacs-essentials)
