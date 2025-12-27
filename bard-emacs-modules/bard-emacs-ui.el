(use-package whitespace
  :ensure nil
  :demand t
  :config
  (setq whitespace-style '(face
                           tabs
                           spaces
                           trailing
                           space-before-tab
                           newline indentation
                           empty space-after-tab
                           space-mark tab-mark))
  :hook
  (prog-mode . whitespace-mode))

(use-package display-line-numbers
  :ensure nil
  :demand t
  :bind
  (("<f12>" . display-line-numbers-mode))
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package keycast
  :ensure t
  :commands (keycast-mode-line-mode keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
  :init
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '("<mouse-event>" "<mouse-movement>" "<mouse-2>" "<drag-mouse-1>" "<wheel-up>" "<wheel-down>" "<double-wheel-up>" "<double-wheel-down>" "<triple-wheel-up>" "<triple-wheel-down>" "<wheel-left>" "<wheel-right>" handle-select-window mouse-set-point  mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil nil))))

(provide 'bard-emacs-ui)
;;; bard-emacs-ui.el ends here
