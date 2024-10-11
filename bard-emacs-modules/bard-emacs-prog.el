(use-package haskell-mode
  :ensure t
  :hook
  ((haskell-mode . interactive-haskell-mode)
   (haskell-mode . haskell-doc-mode)
   (haskell-mode . haskell-indent-mode))
  :config
  (setq haskell-interactive-popup-errors nil))

;; CPP Mode
(use-package emacs
  :config
  (setq-default c-basic-offset 4))

;; Haskell

(use-package emacs
  :config
  (add-to-list 'exec-path "$HOME/.ghcup/bin")
  (add-to-list 'exec-path "/home/bard/.cabal/bin")
  (add-to-list 'exec-path "/home/bard/.local/bin")
  (add-to-list 'exec-path "/home/bard/opt/")
  (let ((bard/ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat bard/ghcup-path ":" (getenv "PATH")))
    (add-to-list 'exec-path bard/ghcup-path)))

;; Lisp
(use-package clojure-mode
  :ensure t
  :bind
  (:map clojure-mode-map
	    ("C-<tab>" . cider-switch-to-repl-buffer))
  :hook
  (clojure-mode . paredit-mode))

(use-package cider
  :ensure t
  :bind
  (:map cider-repl-mode-map
	("C-<tab>" . cider-switch-to-last-clojure-buffer)))

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package geiser
  :ensure t)

(use-package geiser-gauche
  :ensure t)

;; parens packages
(use-package paredit
  :ensure t)

(use-package scheme-mode
  :hook (scheme-mode . paredit-mode))

(use-package lisp-mode
  :hook (lisp-mode . paredit-mode))

(use-package emacs-lisp-mode
  :hook (emacs-lisp-mode . paredit-mode))

(use-package c++-mode
  :bind
  (:map c++-mode-map
        ("C-c C-c" . project-compile)))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

;; Version control
(define-key global-map (kbd "C-c g") #'magit-status)
(setq magit-repository-directories
        '(("~/Code"          . 1)
          ("~/Repositories"  . 1)
          ("~/dotfiles-stow" . 0)
          ("~/.emacs.d"      . 0)))

(use-package ada-mode
  :load-path "~/.emacs.d/old-ada"
  :bind
  (:map ada-mode-map
	("C-j" . dired-jump)))

(use-package diff-hl
  :ensure t
  :after magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :init
  (add-hook 'prog-mode-hook 'diff-hl-mode))

(provide 'bard-emacs-prog)
