;; |------------------------------------|
;; |             Packages               |
;; |------------------------------------|

(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; writeable grep buffers
(use-package wgrep)

;; preview replace
(use-package iedit)

;; Magit
(use-package magit
  :config
  (define-key global-map (kbd "C-c g") #'magit))

(use-package magit-todos
  :config
  (magit-todos-mode 1))

;; Vertico completion
(use-package vertico
  :init
  (vertico-mode 1))

;; Marginalia - works with vertico
(use-package marginalia
  :init
  (marginalia-mode))

(use-package company
  :init
  (global-company-mode 1))

(use-package consult
  :config
  (global-set-key (kbd "C-x b") nil)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-c s") #'consult-line)
  (global-set-key (kbd "C-c C-s") nil)
  (global-set-key (kbd "C-z s") #'consult-ripgrep))

(use-package yasnippet
  :init
  (yas-global-mode)
)

(use-package yasnippet-snippets
)

(use-package which-key
  :init
  (which-key-mode 1)
)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package elfeed
  :config
  (global-set-key (kbd "C-c e") 'elfeed)
  (setq elfeed-search-filter "+unread -academia"))

(use-package elfeed-org
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/feeds.org"
				   "~/.emacs.d/youtube.org")))

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/powerline-default-separator 'box))

(use-package hl-todo
  :init
  (global-hl-todo-mode t))

(use-package pdf-tools)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package clojure-mode)

(use-package cider)

(use-package clojure-snippets)

(use-package haskell-mode)

(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package smartparens
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
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

(use-package tramp)

(use-package circe)

(use-package spacious-padding
  :init
  (spacious-padding-mode 1)
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 10
	 :header-line-width 4
	 :mode-line-width 6
	 :tab-width 4
	 :right-divider-width 30
	 :scroll-bar-width 8)))

(use-package darkroom
  :bind
  (("<f9>" . bard/darkroom-toggle))
  :config
  (setq darkroom-margins 'darkroom-guess-margins)
  (setq darkroom-text-scale-increase 1.2)
  (defun bard/darkroom-toggle ()
    (interactive)
    (if (equal darkroom-tentative-mode nil)
	(progn
	  (visual-line-mode t)
	  (darkroom-tentative-mode t)
	  (setq cursor-type 'bar)
	)
      (progn
	(darkroom-tentative-mode 0)
	(mixed-pitch-mode 0)
	(visual-line-mode nil)
	;; (fringe-mode nil)
	(setq cursor-type 'box)))))

(provide 'bard-emacs-package.el)
