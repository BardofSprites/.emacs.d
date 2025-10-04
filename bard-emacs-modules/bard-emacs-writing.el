;;; Input related settings

(use-package emacs
  :ensure nil
  :demand t
  :bind
  (("C-x i" . insert-char)
   ("M-z"   . zap-to-char)
   ("<f10>"  . toggle-input-method))
  :config
  ;; Sentence size
  (setq sentence-end-double-space nil)

  ;; Keyboard things
  (setq default-input-method "cyrillic-yawerty")
  (setq default-transient-input-method "cyrillic-yawerty"))

(use-package text-mode
  :ensure nil
  :hook
  (text-mode . flyspell-mode))

;; Tab settings
(use-package emacs
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 4
                indent-tabs-mode nil))

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

;; snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)
  )

(use-package yasnippet-capf
  :ensure t
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;; Notes
(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/Notes/denote/")
  (setq denote-journal-extras-directory "~/Notes/journal")
  (setq denote-rename-buffer-format "[Note] %t %b")
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
  (setq denote-templates
        '((default . "Related to — ")
          (physics . "* Introduction\n** Hypothesis\n** Materials\n* Procedure\n* Data\n* Conclusion")
          (writing . "* Prompt\n* Outline\n* Response")))

  (setq denote-save-buffers t)
  (setq denote-prompts '(title keywords))
  (setq denote-sort-dired-extra-prompts nil)
  (setq denote-sort-dired-default-sort-component 'identifier)
  (setq denote-sort-dired-default-reverse-sort nil)

  ;; backlinks sidebar
  (setq denote-backlinks-display-buffer-action
        '((display-buffer-in-direction)
          (direction . right)
          (window-width . 0.33)
          (window-height . fit-window-to-buffer)
          (dedicated . t)))

  (denote-rename-buffer-mode 1)

  (require 'bard-writing)

  (add-hook 'denote-after-new-note-hook #'bard/denote-insert-id-at-top)

  :hook
  (dired-mode . denote-dired-mode)
  
  :bind
  (("C-c n n" . denote)
   ("C-c n <TAB>" . denote-region)
   ("C-c n d" . denote-sort-dired)
   ("C-c n r" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-rename-file-keywords)
   ("C-c n i" . denote-link)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . bard/consult-buffer-notes)   ; notes buffer
   ("C-c n B" . bard/ibuffer-notes)          ; notes buffer but more
   ("C-c n f" . bard/find-notes-file)        ; notes-find
   ("C-c n g" . bard/search-notes-directory) ; notes-grep
   ("C-c n l" . denote-find-link)
   ("C-c n L" . denote-find-backlink)))

(use-package denote-org
  :ensure t
  )

(use-package denote-silo
  :ensure t
  :config
  (setq denote-silo-directories '("~/Notes/denote"
                                  "~/Notes/Old Notes/"))
  )

(use-package denote-sequence
  :ensure t
  :config
  (require 'bard-writing)
  :bind
  ("C-c n N" . denote-sequence)
  ("C-c n D" . denote-sequence-dired)
  ("C-c n <SPC>" . denote-sequence-region))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Notes/denote"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	     ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . org-roam-node-find))
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :ensure t)


;;; Focus mode for writing

;; Center line scrolling for focused writing
(use-package emacs
  :config
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
  (defun bard/cursor-centered-p ()
    "Check if `bard/scroll-center-cursor-mode` is currently active."
    (bound-and-true-p bard/scroll-center-cursor-mode))
  :bind
  (("C-c L" . bard/scroll-center-cursor-mode)))

(use-package olivetti
  :ensure t
  :config
  (setq olivetti-minimum-body-width 90)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :hook
  ((olivetti-mode-on . (lambda () (olivetti-set-width 90)))
   ))

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

  (defun bard/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top
  :hook
  ((logos-page-motion . bard/logos--recenter-top))
  :hook
  ((org-mode . logos-focus-mode)
   (markdown-mode . logos-focus-mode))
  :bind
  (("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   ("<f9>" . logos-focus-mode)
   ("C-x n n" . logos-narrow-dwim)))

(use-package citar
  :ensure t
  :bind
  ("C-c n c" . citar-open)
  :config
  (setq citar-bibliography '("~/Documents/bib/references.bib"))
  (setq org-cite-global-bibliography citar-bibliography
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)

  (setq citar-notes-paths '("~/Notes/denote"))
  (setq citar-library-paths '("~/Documents/Research Articles/"))
  ;; (setq citar-file-open-functions 'find-file)
  :hook
  (org-mode . citar-capf-setup)
  :bind (("C-c i" . citar-insert-citation))
  )

(use-package citar-denote
  :ensure t
  :config
  (citar-denote-mode t)
  )

(use-package citar-embark
  :ensure t
  )

;; Org Roam (trying this out again?)
;;;; Org Roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Notes/denote"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	     ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :ensure t
  :bind
  ("C-c n u" . org-roam-ui-open))

(provide 'bard-emacs-writing)
