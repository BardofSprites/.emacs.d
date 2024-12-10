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
(repeat-mode t)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)

;; Add the directories to the load path
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("bard-elisp" "bard-emacs-modules" "old-ada"))

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
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

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

(require 'bard-emacs-anki)
(require 'bard-emacs-calendar)
(require 'bard-emacs-completion)
(require 'bard-emacs-dired)
(require 'bard-emacs-email)
(require 'bard-emacs-emms)
(require 'bard-emacs-eshell)
(require 'bard-emacs-essentials)
(require 'bard-emacs-git)
(require 'bard-emacs-keyboard)
(require 'bard-emacs-modeline)
(require 'bard-emacs-org)
(require 'bard-emacs-prog)
(require 'bard-emacs-theme)
(require 'bard-emacs-ui)
(require 'bard-emacs-web)
(require 'bard-emacs-window)
(require 'bard-emacs-writing)

(defun bard/setup-initial-buffers ()
  "Set up initial tabs for Org Agenda, EMMS, and todo.org file."
  (interactive)
  (bard/default-agenda)
  (emms)
  (find-file "~/Notes/denote/todo.org")
  (unless (bound-and-true-p tab-bar-mode)
    (tab-bar-mode 1))
  (tab-bar-new-tab)
  (switch-to-buffer "*Org Agenda*")
  (tab-bar-new-tab)
  (switch-to-buffer " *EMMS Playlist*")
  (tab-bar-new-tab)
  (switch-to-buffer "*scratch*"))

(bard/setup-initial-buffers)

(provide 'init)
(put 'eshell 'disabled nil)
