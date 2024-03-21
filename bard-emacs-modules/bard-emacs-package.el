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
(use-package wgrep
  :ensure t)

;; preview replace
(use-package iedit
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
  (global-set-key (kbd "C-z s") #'consult-ripgrep))

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

(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-c e") 'elfeed)
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

(use-package notmuch
  :ensure t
  :config
  (define-key global-map (kbd "C-c m") #'notmuch))

(use-package notmuch-indicator
  :ensure t
  :config)

(use-package tmr
  :ensure t
  :config
    ;; Set to nil to disable the sound
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
  ;; Desktop notification urgency level
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list 'tmr-description-history)
  (define-key global-map (kbd "C-c t l") 'tmr-tabulated-view)
  (define-key global-map (kbd "C-c t T") #'tmr-with-description)
  (define-key global-map (kbd "C-c t l") #'tmr-tabulated-view)
  (define-key global-map (kbd "C-c t c") #'tmr-clone)
  (define-key global-map (kbd "C-c t k") #'tmr-cancel)
  (define-key global-map (kbd "C-c t s") #'tmr-reschedule)
  (define-key global-map (kbd "C-c t e") #'tmr-edit-description)
  (define-key global-map (kbd "C-c t r") #'tmr-remove)
  (define-key global-map (kbd "C-c t R") #'tmr-remove-finished))

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode t))

(use-package pdf-tools
  :ensure t)

(use-package org-timeblock
  :ensure t
  :config
  (define-key global-map (kbd "C-z c") 'org-timeblock))

(use-package orthodox-christian-new-calendar-holidays
  :ensure t
  :config
  (setq holiday-other-holidays (append holiday-other-holidays orthodox-christian-new-calendar-holidays))

  (setq holiday-bahai-holidays nil
	holiday-christian-holidays nil
	holiday-islamic-holidays nil))

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

(use-package tramp
  :ensure t)

(use-package circe
  :ensure t)

(use-package spacious-padding
  :ensure t
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
  :ensure t
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
