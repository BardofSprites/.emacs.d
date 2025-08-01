(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-interactive-popup-errors nil))

;; CPP Mode
(use-package emacs
  :config
  (setq-default c-basic-offset 4)
  (setq c-default-style '((c-mode . "gnu")
                          (java-mode . "java")
                          (awk-mode . "awk"))))

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
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package geiser
  :ensure t)

(use-package geiser-gauche
  :ensure t)

;; parens packages

(use-package c++-mode
  :ensure nil
  :bind
  (:map c++-mode-map
        ("C-c C-c" . project-compile))
  :config
  (defun bard/c++-completion-or-indent ()
    "Complete if point is at a symbol; otherwise, indent."
    (interactive)
    (if (or (completion-at-point) (looking-at "\\_>"))
        (completion-at-point)
      (c-indent-line-or-region)))

  (defun bard/setup-c++-mode ()
    "Configure completion behavior for `c++-mode'."
    (local-set-key (kbd "<tab>") 'bard/c++-completion-or-indent))

  (add-hook 'c++-mode-hook #'bard/setup-c++-mode)
  (add-hook 'c-mode-hook #'bard/setup-c++-mode)
  )

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
                (ggtags-mode 1)
                (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))
            ))

(use-package compile
  :ensure nil
  :defer 2
  :config
  (require 'bard-compile)
  (setq compilation-scroll-output t
        compilation-auto-jump-to-first-error nil)
  )

;; Version control
(use-package magit
  :ensure t
  :config
  (setq magit-repository-directories
        '(("~/Code"                    . 1)
          ("~/Repositories"            . 1)
          ("~/dotfiles-stow"           . 0)
          ("~/.emacs.d"                . 0)
          ("~/Pictures/wallpaper"      . 0)))
  :bind ("C-c g" . magit-status)
  )

;; (use-package ada-mode
;;   :load-path "~/.emacs.d/old-ada"
;;   :bind
;;   (:map ada-mode-map
;; 	("C-j" . dired-jump)))

(provide 'bard-emacs-prog)
