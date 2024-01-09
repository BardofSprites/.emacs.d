;; |------------------------------------|
;; |             Packages               |
;; |------------------------------------|

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Ef-themes
(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-headings
      '((1 variable-pitch 1.5)
        (2 regular 1.3)
        (3 1.1)
        (agenda-date 1.3)
        (agenda-structure variable-pitch light 1.8)
        (t variable-pitch)))
  (defun bard/ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("WAIT" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("THEM" . ,magenta)
              ("PROG" . ,cyan-warmer)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("KLUDGE" . ,cyan)
              ("HACK" . ,cyan)
              ("TEMP" . ,red)
              ("FIXME" . ,red-warmer)
              ("XXX+" . ,red-warmer)
	      ("KILLED" . ,cyan)
              ("REVIEW" . ,red)
              ("DEPRECATED" . ,yellow)))))

  (add-hook 'ef-themes-post-load-hook #'bard/ef-themes-hl-todo-faces)
  (define-key global-map (kbd "M-<f5>") #'ef-themes-toggle)
  (setq ef-themes-to-toggle '(ef-melissa-dark ef-melissa-light))
  (load-theme 'ef-melissa-dark t))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-headings
      '((1 . (variable-pitch 1.5))
        (2 . (1.3))
        (agenda-date . (1.3))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.1))))
  (defun bard/modus-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Modus themes colors.
The exact color values are taken from the active Ef theme."
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("WAIT" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("THEM" . ,magenta)
              ("PROG" . ,cyan-warmer)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("KLUDGE" . ,cyan)
              ("HACK" . ,cyan)
              ("TEMP" . ,red)
              ("FIXME" . ,red-warmer)
              ("XXX+" . ,red-warmer)
              ("REVIEW" . ,red)
	      ("KILLED" . ,cyan)
              ("DEPRECATED" . ,yellow)))))

  (add-hook 'modus-themes-post-load-hook #'bard/modus-themes-hl-todo-faces)
  ;; (load-theme 'modus-vivendi t)
  (setq modus-themes-to-toggle '(modus-vivendi modus-operandi-tinted))
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package rainbow-mode
  :ensure t)

(use-package mixed-pitch
  :ensure t
  :hook
  (olivetti-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-cursor-type 'box))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode 1))

;; Vertico completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; Marginalia - works with vertico
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode 1))

(use-package consult
  :ensure t
  :config
  (global-set-key (kbd "C-x b") nil)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-c s") #'consult-line)
  (global-set-key (kbd "C-c C-s") nil)
  (global-set-key (kbd "C-c C-s") #'consult-ripgrep))

(use-package orderless
  :ensure t)

(use-package yasnippet
  :init
  (yas-global-mode)
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package which-key
  :init
  (which-key-mode 1)
  :ensure t)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Notes/Org-Roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :ensure t)

(use-package org-fragtog
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice 'dashboard-open)
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/img/catwithscarf.jpg")
  (setq dashboard-banner-logo-width 50)
  (setq dashboard-banner-logo-height 50)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 5)
			  (bookmarks . 5)))
  (setq dashboard-banner-logo-title "An Old Farmer's Smile")
  (setq dashboard-set-footer nil))

(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-search-filter "+unread -academia"))

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/feeds.org"
				   "~/.emacs.d/youtube.org")))

(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/powerline-default-separator 'box))

(use-package org-cliplink
  :ensure t)

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode t))

(use-package pdf-tools
  :ensure t)

(use-package toc-org
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-snippets
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package tuareg
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :bind
  (("C-<down>" . sp-down-sexp))
  ("C-<up>"    . sp-up-sexp)
  ("M-<down>"  . sp-backward-up-sexp)
  ("M-<up>"    . sp-backward-up-sexp)
  ("C-M-a"     . sp-beginning-of-sexp)
  ("C-M-e"     . sp-end-of-sexp))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

(use-package emacs-everywhere
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'rainbow-mode)
  ;; (diminish 'helm-mode)
  (diminish 'flycheck-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode)
  (diminish 'org-roam-ui-mode "ORUI")
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'company-mode)
  (diminish 'whitespace-mode))

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package shell-pop
  :ensure t)

(use-package circe
  :ensure t
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-term-shell "/bin/bash"))

(provide 'bard-emacs-package.el)
