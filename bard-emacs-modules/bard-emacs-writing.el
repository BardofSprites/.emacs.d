;;; Input related settings

(use-package emacs
  :ensure t
  :bind
  (("C-x i" . insert-char))
  :config
  ;;; Sentence size
  (setq sentence-end-double-space nil)

;;; Keyboard things
  (setq default-input-method "cyrillic-yawerty")
  (setq default-transient-input-method "cyrillic-yawerty"))

;; Tab settings
(use-package emacs
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 4
                indent-tabs-mode nil))

(use-package electric
  :hook
  (prog-mode . electric-indent-local-mode))

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
  :hook
  ((dired-mode . denote-dired-mode)

   ;; journalling with timer
   (denote-journal-extras-hook . (lambda ()
                                   (tmr "10" "Journalling"))))

  :bind
  (("C-c n n" . denote-open-or-create)
   ("C-c n N" . denote)
   ("C-c R" . denote-region)
   ("C-c n d" . denote-date)
   ("C-c n o" . denote-sort-dired)
   ("C-c n j" . denote-journal-extras-new-entry)
   ("C-c n r" . denote-rename-file)
   ("C-c n r" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-rename-file-keywords)
   ("C-c n i" . denote-link)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n f" . denote-find-link)
   ("C-c n F" . denote-find-backlink)))


(provide 'bard-emacs-writing)
