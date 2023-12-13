;;; init.el --- init.el -*- lexical-binding: t -*-

;; Author: BardofSprites
;; Maintainer: BardofSprites
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

;; Add the directories to the load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path (expand-file-name "bard-elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "bard-emacs-modules" user-emacs-directory))

;; Function to load all .el files in a directory
(defun load-directory (dir)
  "Load all .el files in the specified directory."
  (dolist (file (directory-files dir t "\\.el$"))
    (message "Loading file: %s" file)
    (load file)))

;; Load all .el files in the bard-elisp directory
(load-directory (expand-file-name "bard-elisp" user-emacs-directory))

;; Load all .el files in the bard-emacs-modules directory
(load-directory (expand-file-name "bard-emacs-modules" user-emacs-directory))

;; Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
      desktop-files-not-to-save "\(^$\\|\\*scratch\\*\\|\\*Messages\\*\\|\\*dashboard\\*\\|\\*Async-native-compile-log\\*|\\*Music\\*)")

;; |------------------------------------|
;; |          General Keybinds          |
;; |------------------------------------|
;; Buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Desktop/session save
(global-set-key (kbd "C-' s") 'desktop-save-in-desktop-dir)
(global-set-key (kbd "C-' r") 'desktop-read)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.6 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(zenburn-theme gruber-darker-theme gruber-darker org-bullets org-bullet gruvbox-theme tuareg emacs-everywhere smartparens smart-parens flycheck vterm yasnippet-snippets which-key vertico use-package toc-org tao-theme sly rainbow-mode projectile pdf-tools org-roam-ui org-cliplink orderless olivetti multiple-cursors modus-themes mixed-pitch marginalia magit hl-todo haskell-mode expand-region elfeed-org elfeed-goodies ef-themes dashboard counsel company clojure-snippets cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-structure ((t (:inherit bold :foreground "#70a89f" :height 1.5 :family "Iosevka Comfy Motion Duo")))))
