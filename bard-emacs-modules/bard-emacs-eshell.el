(use-package eshell
  :ensure t
  :config
  (setq eshell-banner-message "Time for another recreational programming session.\n\n"))

(define-key global-map (kbd "C-z e") #'eshell-switcher)
(with-eval-after-load "esh-mode"
  (define-key eshell-mode-map (kbd "C-c f") #'bard/eshell-find-file-at-point)
  (define-key eshell-mode-map (kbd "C-c h") #'prot-eshell-narrow-output-highlight-regexp)
  (define-key eshell-mode-map (kbd "C-c d") #'prot-eshell-complete-recent-dir)
  (define-key eshell-mode-map (kbd "M-k") #'eshell-kill-input))

(provide 'bard-emacs-eshell.el)
