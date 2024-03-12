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

;; Clipboard saving
(setq x-select-enable-clipboard t)

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

;; Savehist
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 100)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(register-alist kill-ring))
(savehist-mode 1)

;; |------------------------------------|
;; |          General Keybinds          |
;; |------------------------------------|
;; Buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Desktop/session save
(global-set-key (kbd "C-' s") 'desktop-save-in-desktop-dir)
(global-set-key (kbd "C-' r") 'desktop-read)

(electric-pair-mode t)

(setq custom-safe-themes t)

(provide 'init)

;;; init.el ends here
