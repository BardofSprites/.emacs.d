;;; Sentence size
(setq sentence-end-double-space nil)
(define-key org-mode-map (kbd "C-M-a") #'backward-paragraph)
(define-key org-mode-map (kbd "C-M-e") #'forward-paragraph)

;;; Keyboard things
(setq default-input-method "cyrillic-yawerty")
(setq default-transient-input-method "cyrillic-yawerty")

;; Tab settings
(setq tab-always-indent 'complete)
(setq tab-first-completion 'word-or-paren-or-punct)
(setq-default tab-width 4
              indent-tabs-mode nil)

(use-package electric
  :hook
  (prog-mode . electric-indent-local-mode)
  :config
  ;; only indents for programming
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  (electric-indent-mode -1))

(use-package paren
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay))

;; Altcaps
(use-package altcaps
  :ensure t
  :config
  (define-key global-map (kbd "C-x C-a") #'altcaps-dwim))

;; snippets

(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (setq tempel-path "~/.emacs.d/tempel-snippets.el"))

;;; Notes
(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/Notes/denote/")
  (setq denote-journal-extras-directory "~/Notes/journal")
  (setq denote-known-keywords
	'("emacs"
	  "linux"
	  "programming"
	  "org"
	  "school"
	  "language"
	  "history"
	  "biology"
	  ))
  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  ;; journalling with timer
  (add-hook 'denote-journal-extras-hook (lambda ()
                                          (tmr "10" "Journalling")
                                          (bard/scroll-center-cursor-mode t)))

  :bind
  (("C-c n n" . denote-open-or-create)
   ("C-c n N" . denote)
   ("C-c R" . denote-region)
   ("C-c n d" . denote-date)
   ("C-c n o" . denote-sort-dired)
   ("C-c n j" . denote-journal-extras-new-entry)
   ("C-c n r" . denote-rename-file)
   ("C-c n r" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-keywords-add)
   ("C-c n K" . denote-keywords-remove)
   ("C-c n i" . denote-link)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n f" . denote-find-link)
   ("C-c n F" . denote-find-backlink)))

(use-package denote-explore
  :ensure t)

(use-package denote-menu
  :ensure t
  :bind
  (("C-c n l" . denote-menu-list-notes)))

;;; Focus mode for writing

;; Center line scrolling for focused writing
(define-minor-mode bard/scroll-center-cursor-mode
  "Toggle centered cursor scrolling behavior."
  :init-value nil
  :lighter " S="
  :global nil
  (if bard/scroll-center-cursor-mode
      (setq-local scroll-margin (* (frame-height) 2)
		  scroll-conservatively 0
		  maximum-scroll-margin 0.5)
    (dolist (local '(scroll-preserve-screen-position
		     scroll-conservatively
		     maximum-scroll-margin
		     scroll-margin))
      (kill-local-variable `,local))))

(define-key global-map (kbd "C-c L") #'bard/scroll-center-cursor-mode)

(use-package olivetti
  :ensure t
  :config
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :hook
  ((olivetti-mode-on-hook . (lambda () (olivetti-set-width 100)))
   (olivetti-mode-hook . (lambda () (bard/scroll-center-cursor-mode t)))))

;; narrowing and focus mode
(use-package logos
  :ensure t
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
		logos-hide-mode-line nil
		logos-hide-header-line t
		logos-hide-buffer-boundaries t
		logos-hide-fringe t
		logos-variable-pitch t
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

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-noter-pdftools
  :ensure t)

(provide 'bard-emacs-writing)
