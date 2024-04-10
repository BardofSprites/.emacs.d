(require 'bard-search)

;; Nice scrolling
(pixel-scroll-precision-mode 1)

;;; Editing niceties

(electric-pair-mode t)
;; writeable grep buffers
(use-package wgrep
  :bind
  (:map wgrep-mode-map
	("C-x C-s" . wgrep-save-all-buffers)))

;; preview replace
(use-package iedit)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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
(global-set-key (kbd "C-' s") 'desktop-save-in-desktop-dir)
(global-set-key (kbd "C-' r") 'desktop-read)

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

(define-key global-map (kbd "C-t") #'bard/open-terminal-in-current-directory)
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

;; timer package
(use-package tmr
  :ensure t
  :config
  (setq tmr-sound-file "/home/bard/.local/bin/scripts/bell.mp3")
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list 'tmr-description-history)
  (define-key global-map (kbd "C-c t l") 'tmr-tabulated-view)
  (define-key global-map (kbd "C-c t t") #'tmr)
  (define-key global-map (kbd "C-c t T") #'tmr-with-description)
  (define-key global-map (kbd "C-c t l") #'tmr-tabulated-view)
  (define-key global-map (kbd "C-c t c") #'tmr-clone)
  (define-key global-map (kbd "C-c t k") #'tmr-cancel)
  (define-key global-map (kbd "C-c t s") #'tmr-reschedule)
  (define-key global-map (kbd "C-c t e") #'tmr-edit-description)
  (define-key global-map (kbd "C-c t r") #'tmr-remove)
  (define-key global-map (kbd "C-c t R") #'tmr-remove-finished))

;; running emacs as server
(require 'server)
(setq server-client-instructions nil)
(unless (server-running-p)
  (server-start))

(provide 'bard-emacs-essentials)
