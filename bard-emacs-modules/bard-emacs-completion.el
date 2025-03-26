;; minibuffer completion
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  (setq vertico-scroll-margin 0)
  (setq vertico-cycle t)

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
  )

(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; MCT has a variant of this built-in.
    (defun crm-indicator (args)
      (cons (format "[`completing-read-multiple': %s]  %s"
                    (propertize
                     (replace-regexp-in-string
                      "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                      crm-separator)
                     'face 'error)
                    (car args))
            (cdr args)))

    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay nil)
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package clang-capf
  :ensure t
  :config
  (defun bard/clang-capf-init ()
    "Add `clang-capf' to `completion-at-point-functions'."
    (add-hook 'completion-at-point-functions #'clang-capf nil t))

  (add-hook 'c-mode-hook #'bard/clang-capf-init))

(use-package minibuffer
  :config
;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless))

  (setq completion-category-defaults nil)

  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

(use-package consult
  :ensure t
  :defer 2
  :bind*
  ("C-x r b" . consult-bookmark)
  ("M-g M-g" . consult-goto-line)
  ("C-x b" . consult-buffer)
  ("M-s M-f" . consult-find)
  ("M-s M-g" . consult-grep)
  ("M-s M-h" . consult-history)
  ("M-s M-y" . consult-yank-pop)
  ("M-s M-o" . consult-outline)
  ("M-s M-l" . consult-line)
  ("M-s M-k" . consult-kmacro)
  ("M-s M-r" . consult-register)
  :config
  (setq consult-find-args
        (concat "find . -not ( "
                "-path */.git* -prune "
                "-or -path */.cache* -prune )")))

(use-package embark
  :ensure t
  :bind*
  (("C-," . bard-embark-act-no-quit)
   ("C-." . bard-embark-act-quit))
  :config
  (require 'bard-embark)
  (setq embark-keymap-alist
        '((buffer bard-embark-buffer-map)
          (command bard-embark-command-map)
          (expression bard-embark-expression-map)
          (file bard-embark-file-map)
          (function bard-embark-function-map)
          (identifier bard-embark-identifier-map)
          (package bard-embark-package-map)
          (region bard-embark-region-map)
          (symbol bard-embark-symbol-map)
          (url bard-embark-url-map)
          (variable bard-embark-variable-map)
          (t embark-general-map)))

  (defun bard-embark-act-no-quit ()
    "Call `embark-act' but do not quit after the action."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act)))

  (defun bard-embark-act-quit ()
    "Call `embark-act' and quit after the action."
    (interactive)
    (let ((embark-quit-after-action t))
      (call-interactively #'embark-act))
    (when (and (> (minibuffer-depth) 0)
               (derived-mode-p 'completion-list-mode))
      (abort-recursive-edit)))


  (setq embark-confirm-act-all nil)
  ;; The prot-embark.el has an advice to further simplify the
  ;; minimal indicator.  It shows cycling, which I never want to see
  ;; or do.
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 0.1)
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
  (setq embark-verbose-indicator-buffer-sections '(bindings))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-cycle embark-act-all embark-collect embark-export embark-insert)))

;; Savehist
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 100)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(register-alist kill-ring))
(savehist-mode t)

;;; abbrev-mode
(setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
(setq only-global-abbrevs nil)

(bard/make-abbrev global-abbrev-table
  "meweb" "https://bardman.dev"
  "megit" "https://github.com/BardofSprites"
  "protweb" "https://protesilaos.com/")

(bard/make-abbrev text-mode-abbrev-table
    "asciidoc"       "AsciiDoc"
    "auctex"         "AUCTeX"
    "cafe"           "café"
    "cliche"         "cliché"
    "clojurescript"  "ClojureScript"
    "emacsconf"      "EmacsConf"
    "github"         "GitHub"
    "gitlab"         "GitLab"
    "javascript"     "JavaScript"
    "latex"          "LaTeX"
    "libreplanet"    "LibrePlanet"
    "linkedin"       "LinkedIn"
    "paypal"         "PayPal"
    "sourcehut"      "SourceHut"
    "texmacs"        "TeXmacs"
    "typescript"     "TypeScript"
    "visavis"        "vis-à-vis"
    "vscode"         "Visual Studio Code"
    "youtube"        "YouTube"
    "Результат"      "=Результат Сегодняшний Битвый="
    "asf"            "and so on and so forth"
    "paragraph"      "¶")

(dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
  (add-hook hook #'abbrev-mode))

(remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

(provide 'bard-emacs-completion)
;;; bard-emacs-completion.el ends here
