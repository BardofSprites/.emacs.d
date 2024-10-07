;;; Editing niceties

(use-package emacs
  :bind
  (("C-c r" . jump-to-register))
  :config
  (setq reb-re-syntax 'string))

;; writeable grep buffers
(use-package wgrep
  :ensure t
  :bind
  (:map wgrep-mode-map
	    ("C-x C-s" . wgrep-save-all-buffers)
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit))
  :bind
  (:map grep-mode-map
        ("e" . wgrep-change-to-wgrep-mode)))

;; preview replace
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/always-run-for-all t)
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C" . mc/mark-all-like-this)
   ("C-\"". mc/skip-to-next-like-this)
   ("C-:" . mc/skip-to-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package substitute
  :ensure t
  :bind
  (("C-c s b" . substitute-target-below-point)
   ("C-c s a" . substitute-target-above-point)
   ("C-c s d" . substitute-target-in-defun)
   ("C-c s s" . substitute-target-in-buffer)))

;; Desktop mode/session saving
(setq desktop-path '("~/.emacs.d/desktop")
      desktop-dirname "~/.emacs.d/desktop/"
      desktop-base-file-name "emacs-desktop"
      desktop-save t
      desktop-restore-eager t
      desktop-restore-=frams t
      desktop-restory-in-current-display t
      desktop-files-not-to-save "\(^$\\|\\*scratch\\*\\|\\*Messages\\*\\|\\*dashboard\\*\\|\\*Async-native-compile-log\\*|\\*Music\\*)")

;;; General Keybinds

;; Buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Desktop/session save
;; (desktop-save-mode t)
(global-set-key (kbd "C-z s") 'desktop-save-in-desktop-dir)
(global-set-key (kbd "C-z r") 'desktop-read)

;;; Scratch buffers
;; Text Scratch buffers
(defun bard/new-org-buffer ()
  (interactive)
  (let ((xbuf (generate-new-buffer "*org*")))
    (switch-to-buffer xbuf)
    (funcall (quote org-mode))
    (text-scale-increase 1.5)
    xbuf))
(define-key global-map (kbd "M-=") #'bard/new-org-buffer)

(defun bard/new-plain-buffer ()
  (interactive)
  (let ((xbuf (generate-new-buffer "*plain*")))
    (switch-to-buffer xbuf)
    (text-scale-increase 1.5)
    xbuf))

(define-key global-map (kbd "M--") #'bard/new-plain-buffer)

;; elisp scratch buffer

(defun bard/new-elisp-buffer ()
  (interactive)
  (let ((xbuf (generate-new-buffer "*elisp*")))
    (switch-to-buffer xbuf)
    (funcall (quote emacs-lisp-mode))
    (text-scale-increase 1.5)
    xbuf))

;;; Terminals
(defun bard/open-terminal-in-current-directory ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (term "/bin/bash")))

;; (define-key global-map (kbd "C-t") #'bard/open-terminal-in-current-directory)
(define-key global-map (kbd "C-z t") #'bard/open-terminal-in-current-directory)

(defun bard/open-terminal-emulator ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (start-process "st terminal" nil "st")))

(define-key global-map (kbd "C-z C-t") 'bard/open-terminal-emulator)

(define-key global-map (kbd "C-z C-s") #'bard/new-elisp-buffer)

;;; Time Management
;; Modeline
(setq display-time-format "%Y-%m-%d (%A) %H:%M")
(setq display-time-interval 60)
(setq display-time-default-load-average nil)
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)
(setq display-time-string-forms
      '((propertize
         (format-time-string display-time-format now)
         'face 'display-time-date-and-time
         'help-echo (format-time-string "%a %b %e, %Y" now))
        " "))
(display-time-mode 1)

;; world clock
(setq world-clock-list
      '(("America/New_York" "New York")
	    ("Europe/Moscow" "Moscow")
	    ("Europe/London" "London")
	    ("Asia/Tokyo" "Tokyo")))

(setq world-clock-time-format "%Y-%m-%d %B (%A) %R %Z")

(define-key global-map (kbd "C-c C-w") #'world-clock)

(use-package breadcrumb
  :ensure t
  :hook
  (prog-mode . breadcrumb-local-mode))

(use-package vundo
  :ensure t
  :defer 1
  :bind
  ( :map vundo-mode-map
    ("C-/" . vundo-backward)
    ("C-?" . vundo-forward)
    ("g" . vundo-goto-last-saved)
    ("p" . vundo-backward)
    ("n" . vundo-forward)
    ("f" . vundo-next)
    ("b" . vundo-previous))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)

  (defvar prot/vundo-undo-functions '(undo undo-only undo-redo)
    "List of undo functions to check if we need to visualise the undo ring.")

  (defvar prot/vundo-undo-command #'undo
    "Command to call if we are not going to visualise the undo ring.")

  (defun prot/vundo-if-repeat-undo (&rest args)
    "Use `vundo' if the last command is among `prot/vundo-undo-functions'.
In other words, start visualising the undo ring if we are going
to be cycling through the edits."
    (interactive)
    (if (and (member last-command prot/vundo-undo-functions)
             (not undo-in-region))
        (call-interactively 'vundo)
      (apply args)))

  (mapc
   (lambda (fn)
     (advice-add fn :around #'prot/vundo-if-repeat-undo))
   prot/vundo-undo-functions))

;; running emacs as server
(require 'server)
(setq server-client-instructions nil)
(unless (server-running-p)
  (server-start))

(provide 'bard-emacs-essentials)
