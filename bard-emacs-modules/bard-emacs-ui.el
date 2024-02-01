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
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq frame-title-format "GNU Emacs")
(setq mode-line-misc-info
      (delete (assoc 'minor-mode-alist mode-line-misc-info) mode-line-misc-info))

;; Modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(tool-bar-mode 0)

;; Whitespace, used in bard-emacs-lang.el
(defun bard/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1))

(setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))

;; Modeline
(setq display-time-format "%Y-%m-%d %H:%M")
(setq display-time-default-load-average nil)
(setq display-time-mail-file nil)
(display-time-mode 1)

;; Fonts

;; Removed in favour of fontaine package
;; (set-face-attribute 'default nil :font "Iosevka Comfy" :height 140)
;; (set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy" :height 140)
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Motion" :height 140)

;; (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-14.5"))
(setq fontaine-presets
      '((regular
         :default-height 140
	 :default-family "Iosevka Comfy"
	 :variable-pitch-family "Iosevka Comfy Motion"
	 :variable-pitch-height 1.0
	 :fixed-pitch-family "Iosevka Comfy"
	 :fixed-pitch-height 1.0
	 :bold-weight bold
	 )
        (large
	 :inherit regular
	 :default-height 170
	 )
	(small
	 :inherit regular
	 :default-height 130)
        (t
         :default-family "Monospace"
         )))

;; save file
(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))
;; Mixed pitch on modus and ef themes
(setq modus-themes-mixed-fonts t)
(setq ef-themes-mixed-fonts t)
;; preserve fonts when switching themes
(dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
  (add-hook hook #'fontaine-apply-current-preset))

(define-key global-map (kbd "<f7>") #'fontaine-set-preset)

;; Switching themes
;; (defun bard/disable-all-themes ()
;;   "disable all active themes."
;;   (dolist (i custom-enabled-themes)
;;     (disable-theme i)))

;; (defadvice load-theme (before disable-themes-first activate)
;;   (bard/disable-all-themes))

;; olivetti
(use-package olivetti
  :config
  (defun bard/olivetti-toggle ()
    (interactive)
    "Distraction-free writing environment"
    (if (equal olivetti-mode nil)
        (progn
          (fontaine-set-preset 'large)
          (setq olivetti-body-width 80)
          (set-fringe-mode 0)
          (olivetti-mode t))
      (progn
        (olivetti-mode 0)
        (fontaine-set-preset 'regular)
        (mixed-pitch-mode 0)
        (fringe-mode nil)
        (setq cursor-type 'box))))
  :bind
  (("<f9>" . bard/olivetti-toggle)))

(provide 'bard-emacs-ui)

;;; bard-emacs-ui.el ends here
