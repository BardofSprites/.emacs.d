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
(let ((my-ghcup-path (expand-file-name "~/.ghcup/bin")))
  (setenv "PATH" (concat my-ghcup-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-ghcup-path))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;; Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Fonts
(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140)
(set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Wide" :height 140)

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
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(setq frame-title-format "GNU Emacs")

;; |------------------------------------|
;; |          Modes and Hooks           |
;; |------------------------------------|
;; pair parens and quotes automatically
(electric-pair-mode 1)

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

;; Custom stuff that no one cares about D:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(emms gruber-darker-theme haskell-mode clojure-snippets cider clojure-mode mixed-pitch tao-theme gruber-darker vterm yasnippet-snippets which-key vertico use-package toc-org projectile pdf-tools org-roam org-cliplink orderless olivetti multiple-cursors marginalia magit hl-todo expand-region ef-themes dashboard counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
