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
  :defer 2
  :config
  (global-set-key (kbd "C-x b") nil)
  (global-set-key (kbd "C-x b") #'consult-buffer)
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
;; olivetti
(use-package olivetti
  :config
  (setq olivetti-body-width 120
        olivetti-recall-visual-line-mode-entry-state t))

(use-package logos
  :config
  (defun logos-reveal-entry ()
    "Reveal Org or Outline entry."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-subtree))
     ((or (eq major-mode 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (outline-show-subtree))))

  (setq logos-outlines-are-pages t)

  (setq logos-outline-regexp-alist
      `((emacs-lisp-mode . "^;;;+ ")
        (org-mode . "^\\* +")
        (t . ,(or outline-regexp logos--page-delimiter))))

  (setq-default logos-hide-cursor nil
              logos-hide-mode-line t
              logos-hide-header-line t
              logos-hide-buffer-boundaries t
              logos-hide-fringe t
              logos-variable-pitch t
              logos-buffer-read-only nil
              logos-scroll-lock nil
              logos-olivetti t)
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))

  (defun bard/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'bard/logos--recenter-top))

(provide 'bard-emacs-package.el)
