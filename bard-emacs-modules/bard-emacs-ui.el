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
  :ensure t
  :init
   (spacious-padding-mode t)
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 10
	 :header-line-width 4
	 :mode-line-width 6
	 :tab-width 4
	 :right-divider-width 1
	 :left-fringe-width 10
	 :right-fringe-width 10
	 :scroll-bar-width 0)))

;; Todo Keywords highlighting, colors defined in bard-emacs-theme.el
(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode t))

;; Show avaiable key chords
(use-package which-key
  :init
  (which-key-mode 1))

;;; Tab bars
;; taken from https://www.jamescherti.com/emacs-tab-bar-vim-style-colors/
(defun bard/tab-bar-vim-name-format-function (tab i)
  "Add a space on the sides of every tab."
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat " "
             (if tab-bar-tab-hints (format "%d " i) "")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p 'non-selected 'selected)))
                      tab-bar-close-button)
                 "")
             " ")
     'face (funcall tab-bar-tab-face-function tab))))

(defun bard/tab-bar-vim-like-colors ()
  "Apply Vim-like color themes to Emacs tab bars."
  (let* ((fallback-light "white")
         (fallback-dark "#333333")
         (bg-default (or (face-attribute 'default :background) fallback-light))
         (fg-default (or (face-attribute 'default :foreground) fallback-dark))
         (bg-modeline-inactive (or (face-attribute 'mode-line-inactive :background)
                                   fallback-dark))
         (fg-modeline-inactive (or (face-attribute 'mode-line-inactive :foreground)
                                   fallback-light))
         (bg-tab-inactive bg-modeline-inactive)
         (fg-tab-inactive fg-modeline-inactive)
         (fg-tab-active fg-default)
         (bg-tab-active bg-default))
    (setq tab-bar-tab-name-format-function #'bard/tab-bar-vim-name-format-function)
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
    (setq tab-bar-separator "\u200B")  ;; Zero width space to fix color bleeding
    (setq tab-bar-tab-hints nil)  ;; Tab numbers of the left of the label
    (setq tab-bar-new-button-show nil)
    (setq tab-bar-close-button-show nil)
    (setq tab-bar-auto-width nil)
    (custom-set-faces
     ;; The tab bar's appearance
     `(tab-bar
       ((t (:background ,bg-tab-inactive
                        :foreground ,fg-tab-inactive
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))
     ;; Inactive tabs
     `(tab-bar-tab-inactive
       ((t (:background ,bg-tab-inactive
                        :foreground ,fg-tab-inactive
                        :box (:line-width 3 :color ,bg-tab-inactive :style nil)))))
     ;; Active tab
     `(tab-bar-tab
       ((t (:background ,bg-tab-active :foreground ,fg-tab-active
                        :box (:line-width 3 :color ,bg-tab-active :style nil))))))))

;; Customize the appearance of the tab bar
;; Make sure to load your theme using 'load-theme' before
;; calling 'bard/tab-bar-vim-like-colors'.
(tab-bar-mode 1)
(bard/tab-bar-vim-like-colors)

(provide 'bard-emacs-ui)

;;; bard-emacs-ui.el ends here
