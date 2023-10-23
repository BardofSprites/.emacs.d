;;; bard-editing.el --- My custom editing functions and keybinds -*- lexical-binding: t -*-

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
;; Custom editing functions with intended use of my personal GNU Emacs configuration
;;; Code:

;; Surround region with character
(defun bard/wrap-text-with-markers (start-marker end-marker marker)
  "Surround marked text with any character."
  (interactive "r\nsEnter marker (e.g., \"): ")
  (save-excursion
    (goto-char end-marker)
    (insert marker)
    (goto-char start-marker)
    (insert marker)))

;;(global-set-key (kbd "C-c C-s") 'bard/wrap-text-with-markers)

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

;;; bard-editing.el ends here
