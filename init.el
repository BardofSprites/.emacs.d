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

;; fix keymap
(global-set-key (kbd "C-z") nil)

;; Add the directories to the load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/old-ada/")
(add-to-list 'load-path (expand-file-name "bard-elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "bard-emacs-modules" user-emacs-directory))

;; Function to load all .el files in a directory
(defun load-directory (dir)
  "Load all .el files in the specified directory."
  (dolist (file (directory-files dir t "\\.el$"))
    (message "Loading file: %s" file)
    (load file)))

;; Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Clipboard saving
(setq select-enable-clipboard t)

;; custom file is evil
(setq custom-file (make-temp-file "emacs-custom-"))

;; No Backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-kill-emacs 'y-or-n-p)

;; native comp enabled
(when (native-comp-available-p)
  (setq native-compile-prune-cache t))

;; enable/disable commands
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;;; Packages
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Package cache
(setq package-enable-at-startup t)

;;; MACROS

(defmacro bard/make-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of (i) string pairs mapping the
abbreviation to its expansion or (ii) a string and symbol pair
making an abbreviation to a function."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(if (abbrev-table-p ,table)
       (progn
         ,@(mapcar
            (lambda (pair)
              (let ((abbrev (nth 0 pair))
                    (expansion (nth 1 pair)))
                (if (stringp expansion)
                    `(define-abbrev ,table ,abbrev ,expansion)
                  `(define-abbrev ,table ,abbrev "" ,expansion))))
            (seq-split definitions 2)))
     (error "%s is not an abbrev table" ,table)))

;; Load all .el files in the bard-elisp directory
(load-directory (expand-file-name "bard-elisp" user-emacs-directory))

;; Load all .el files in the bard-emacs-modules directory
(load-directory (expand-file-name "bard-emacs-modules" user-emacs-directory))

(load-directory (expand-file-name "old-ada" user-emacs-directory))

(provide 'init)
