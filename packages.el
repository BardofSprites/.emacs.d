;; |------------------------------------|
;; |             Packages               |
;; |------------------------------------|

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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
  (load-theme 'ef-cyprus t)
  (define-key global-map (kbd "<f5>") #'ef-themes-toggle)
  (setq ef-themes-to-toggle '(ef-autumn ef-cyprus)))
  ;; (setq ef-themes-headings
  ;;     '((0 variable-pitch 1.8)
  ;;       (1 variable-pitch 1.3)
  ;;       (2 regular 1.2)
  ;;       (3 1.1)
  ;;       (agenda-structure variable-pitch 1.5)
  ;;       (t variable-pitch)))

(use-package rainbow-mode
  :ensure t)

(use-package mixed-pitch
  :ensure t
  :hook
  (olivetti-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-cursor-type 'box))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package projectile
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (setq councel-rg-base-command "rg --no-heading --colors always %s .")
  (setq counsel-rg-command-prefix "rg --no-heading --color always %s ."))

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/.emacs.d/catwithscarf.jpg")
    (setq dashboard-banner-logo-width 50)
    (setq dashboard-banner-logo-height 50)
    (setq dashboard-center-content t)
    (setq dashboard-items '((recents   . 5)
			    (bookmarks . 5)
			    (projects  . 5)))
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

;; Distraction-free writing
(use-package olivetti
    :config
    (defun distraction-free ()
      "Distraction-free writing environment using Olivetti package."
      (interactive)
      (if (equal olivetti-mode nil)
          (progn
            (window-configuration-to-register 1)
            (delete-other-windows)
            (text-scale-set 0.2)
	    (setq olivetti-body-width 120)
            (olivetti-mode t))
        (progn
          (if (eq (length (window-list)) 1)
              (jump-to-register 1))
          (olivetti-mode 0)
          (text-scale-set 0))))
    :bind
    (("<f9>" . distraction-free)))

(use-package org-cliplink
  :ensure t)

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode t)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" error bold)
          ("FIXME" error bold)
          ("WAIT" warning bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold))))

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
