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

(use-package tempel
  :ensure t
  :demand t
  ;; Require trigger prefix before template name when completing.
  :bind (("M-i" . tempel-complete) ;; Alternative tempel-expand
         ("M-S-i" . tempel-insert))
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

  :hook
  ((conf-mode . tempel-setup-capf)
   (prog-mode . tempel-setup-capf)
   (text-mode . tempel-setup-capf))
  :config
  (setq tempel-path "~/.emacs.d/tempel-snippets.el"))

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
  (setq denote-prompts '(title keywords signature))
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
  (defun bard/find-notes-file ()
    (interactive)
    (let ((consult-find-args "find . -name \"*.org\"-not ( -path */.git* -prune -or -path */.cache* -prune )"))
      (consult-find "~/Notes/denote")))
  (defun bard/search-notes-directory ()
    (interactive)
    (consult-grep "~/Notes/denote"))

  :hook
  (dired-mode . denote-dired-mode)

  ;; journalling with timer
  (denote-journal-extras-hook . (lambda ()
                                  (tmr "10" "Journalling")
                                  (bard/scroll-center-cursor-mode t)))
  :bind
  (("C-c n n" . denote)
   ("C-c n <TAB>" . denote-region)
   ("C-c n o" . denote-sort-dired)
   ("C-c n j" . denote-journal-extras-new-entry)
   ("C-c n r" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-rename-file-keywords)
   ("C-c n i" . denote-link)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
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
  ("C-c n O" . denote-sequence-dired)
  ("C-c n <SPC>" . denote-sequence-region))

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

(provide 'bard-emacs-writing)
