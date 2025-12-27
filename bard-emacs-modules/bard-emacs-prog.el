(use-package electric
  :hook
  (prog-mode . electric-indent-local-mode)
  (prog-mode . electric-pair-local-mode))

(use-package paren
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay))

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-interactive-popup-errors nil))

(use-package c-mode
  :config
  (setq-default c-basic-offset 4)
  (setq c-default-style '((c-mode . "gnu")
                          (java-mode . "java")
                          (awk-mode . "awk"))))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)
                (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))))

(use-package compile
  :ensure nil
  :defer 2
  :config
  (require 'bard-compile)
  (setq compilation-scroll-output t
        compilation-auto-jump-to-first-error nil))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package geiser
  :ensure t)

(use-package geiser-gauche
  :ensure t)

;; parens packages

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

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
  :bind ("C-c g" . magit-status))

;; (use-package ada-mode
;;   :load-path "~/.emacs.d/old-ada"
;;   :bind
;;   (:map ada-mode-map
;; 	("C-j" . dired-jump)))

(provide 'bard-emacs-prog)
