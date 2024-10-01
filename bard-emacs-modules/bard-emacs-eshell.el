(use-package eshell
  :ensure nil
  :bind
  (("C-z e" . eshell-switcher))
  :config
  (require 'bard-eshell)
  ;; (setq eshell-banner-message "Time for another recreational programming session.\n\n")
  (setq eshell-banner-message
        '(format "%s %s\n %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)
		 (propertize "Time for another recreational programming session."
			         'face 'warning)))
  (setq bard/eshell-aliases
        '((g   . magit)
	      (gl  . magit-log)
	      (d   . dired)
	      (o   . find-file)
          (oo . find-file-other-window)
	      (vim . find-file)
	      (l  . (lambda () (eshell/ls '-la)))
	      (eshell/clear . eshell/clear-scrollback)))

  (mapc (lambda (alias)
	      (defalias (car alias) (cdr alias)))
        bard/eshell-aliases))

(use-package eshell
  :ensure nil
  :after esh-mode
  :bind
  (:map eshell-mode-map
        ("C-c C-e" . prot-eshell-export)
        ("M-k"     . eshell-kill-input)
        ("C-c C-d"   . prot-eshell-complete-recent-dir)
        ("C-c C-h"   . prot-eshell-narrow-output-highlight-regexp)
        ("C-c C-f"   . bard/eshell-find-file-at-point)))

(provide 'bard-emacs-eshell)
