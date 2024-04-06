;;; bard-emacs-ui.el --- ui configuration for emacs -*- lexical-binding: t -*-

;; Author: BardofSprites
;; Maintainer: BardofSprites
;; Version: 0.1.0
;; Package-Requires: (emacs "28.2")

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

;;; Code:

;; Variables
(setq display-line-numbers-type 'relative)

;; Whitespace, used in bard-emacs-lang.el
(defun bard/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1))

(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))

;; display line numbers custom mode
(define-minor-mode bard/display-line-numbers-mode
  "Toggle `display-line-numbers-mode' and  `hl-line-mode' in tandem"
  :init-value nil
  :global nil
  (if (bard/display-line-numbers-mode
       (progn
	 (display-line-numbers-mode 1)
	 (hl-line-mode 1))
       (display-line-numbers-mode -1)
       (hl-line-mode -1))))

;; padding for everything
(use-package spacious-padding
  :init
  (spacious-padding-mode 1)
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 10
	 :header-line-width 4
	 :mode-line-width 6
	 :tab-width 4
	 :right-divider-width 1
	 :left-fringe-width 10
	 :right-fringe-width 10
	 :scroll-bar-width 8)))

;; Todo Keywords highlighting, colors defined in bard-emacs-theme.el
(use-package hl-todo
  :init
  (global-hl-todo-mode t))

;; Show avaiable key chords
(use-package which-key
  :init
  (which-key-mode 1))

(provide 'bard-emacs-ui)

;;; bard-emacs-ui.el ends here
