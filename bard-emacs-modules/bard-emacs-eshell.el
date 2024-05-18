(use-package eshell
  :config
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

(define-key global-map (kbd "C-z e") #'eshell-switcher)
(with-eval-after-load "esh-mode"
  (define-key eshell-mode-map (kbd "C-c f") #'bard/eshell-find-file-at-point)
  (define-key eshell-mode-map (kbd "C-c h") #'prot-eshell-narrow-output-highlight-regexp)
  (define-key eshell-mode-map (kbd "C-c d") #'prot-eshell-complete-recent-dir)
  (define-key eshell-mode-map (kbd "M-k") #'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-c C-e") #'prot-eshell-export))

(provide 'bard-emacs-eshell)
