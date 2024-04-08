;;; Keyboard things
(setq default-input-method "cyrillic-yawerty")
(setq default-transient-input-method "cyrillic-yawerty")

(use-package denote
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

;;; Focus mode for wrigin

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
  :config
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  :hook
  ((olivetti-mode-on-hook . (lambda () (olivetti-set-width 100)))
   (olivetti-mode-hook . (lambda () (bard/scroll-center-cursor-mode t)))))

;; narrowing and focus mode
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
		;; logos-hide-mode-line t
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

(provide 'bard-emacs-writing)
