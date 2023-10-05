;; |------------------------------------|
;; |            Gen Config              |
;; |------------------------------------|

;; Loading all other files
(setq my-config-files '("~/.emacs.d/packages.el"
                        "~/.emacs.d/org.el"
			"~/.emacs.d/haskell.el"))

(dolist (config-file my-config-files)
  (load config-file))

;; GHC Path
;; (let ((my-ghcup-path (expand-file-name "~/.ghcup/bin")))
;;   (setenv "PATH" (concat my-ghcup-path ":" (getenv "PATH")))
;;   (add-to-list 'exec-path my-ghcup-path))

;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;   (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))

;; Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Fonts
(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140)
(set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Wide" :height 140)

(add-to-list 'default-frame-alist '(font . "Iosevka Comfy-14.5"))

;; No Backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-kill-emacs 'y-or-n-p)

;; Desktop mode/session saving
(setq desktop-path '("~/.emacs.d/desktop")
      desktop-dirname "~/.emacs.d/desktop/"
      desktop-base-file-name "emacs-desktop"
      desktop-save t
      desktop-restore-eager t
      desktop-restore-=frams t
      desktop-restory-in-current-display t
      desktop-files-not-to-save "\(^$\\|\\*scratch\\*\\|\\*Messages\\*\\|\\*dashboard\\*\\|\\*Async-native-compile-log\\*)")

;; |------------------------------------|
;; |             UI Config              |
;; |------------------------------------|
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(setq frame-title-format "GNU Emacs")
(setq display-line-numbers-type 'relative)

;; |------------------------------------|
;; |          Modes and Hooks           |
;; |------------------------------------|
;; pair parens and quotes automatically
(electric-pair-mode 1)
(defun bard/common-modes-hook ()
  "Commonly used modes, bundled in one hook"
  (display-line-numbers-mode 1)
  (hl-todo-mode 1))

(add-hook 'org-mode-hook 'bard/common-modes-hook)
(add-hook 'fundamental-mode-hook 'bard/common-modes-hook)
(add-hook 'emacs-lisp-mode-hook 'bard/common-modes-hook)
(add-hook 'haskell-mode-hook 'bard/common-modes-hook)
(add-hook 'clojure-mode-hook 'bard/common-modes-hook)

;; |------------------------------------|
;; |            Keybinds                |
;; |------------------------------------|

;; Org Agenda
(defun bard/primary-agenda ()
  "For viewing my custom agenda"
  (interactive)
  (org-agenda nil "A")
  (delete-other-windows))

(global-set-key (kbd "C-' a") 'bard/primary-agenda)

;; Surround region with character
(defun bard/wrap-text-with-markers (start-marker end-marker marker)
  "Surround marked text with any character."
  (interactive "r\nsEnter marker (e.g., \"): ")
  (save-excursion
    (goto-char end-marker)
    (insert marker)
    (goto-char start-marker)
    (insert marker)))

(global-set-key (kbd "C-c s") 'bard/wrap-text-with-markers)

(defun bard/copy-current-line ()
  "Copy the current line."
  (interactive)
  (let ((line-text (buffer-substring (line-beginning-position) (line-end-position))))
    (kill-new line-text)
    (message "Copied current line")))

(global-set-key (kbd "C-c l") 'bard/copy-current-line)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\\")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Buffer switching
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-,") 'previous-buffer)

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Org Cliplink
(global-set-key (kbd "C-x p i") 'org-cliplink)

;; Desktop/session save
(global-set-key (kbd "C-' s") 'desktop-save-in-desktop-dir)
(global-set-key (kbd "C-' r") 'desktop-read)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elfeed-goodies elfeed-org elfeed rainbow-mode vterm yasnippet-snippets which-key vertico use-package toc-org tao-theme projectile pdf-tools org-roam org-cliplink orderless olivetti multiple-cursors mixed-pitch marginalia magit hl-todo haskell-mode expand-region ef-themes dashboard counsel company clojure-snippets cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
