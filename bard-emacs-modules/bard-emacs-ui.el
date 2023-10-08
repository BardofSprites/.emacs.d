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

;; Modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(tool-bar-mode 0)

;; Fonts
(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy" :height 140)
(set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Wide" :height 140)

(add-to-list 'default-frame-alist '(font . "Iosevka Comfy-14.5"))

;; Common UI hook
(defun bard/text-hook ()
  (bard/olivetti-toggle)
  (display-line-numbers-mode 0)
  (hl-todo-mode 1))

;; olivetti
(use-package olivetti
  :config
  (defun bard/olivetti-toggle-interactive ()
    (interactive)
    "Distraction-free writing environment"
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-set 0.2)
          (setq olivetti-body-width 100)
          (olivetti-mode t))
      ;; TODO turn off line numbers
      (progn
        (if (eq (length (window-list)) 1)
            (jump-to-register 1))
        (olivetti-mode 0)
        (text-scale-set 0)
        (mixed-pitch-mode 0)
	;; TODO turn restore line numbers
        (setq cursor-type 'box))))
  (defun bard/olivetti-toggle ()
    "Distraction-free writing environment"
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-set 0.2)
          (setq olivetti-body-width 100)
	  (display-line-numbers-mode 0)
          (olivetti-mode t))
      (progn
        (if (eq (length (window-list)) 1)
            (jump-to-register 1))
        (olivetti-mode 0)
        (text-scale-set 0)
        (mixed-pitch-mode 0)
        (setq cursor-type 'box))))
  :bind
  (("<f9>" . bard/olivetti-toggle-interactive)))

(provide 'bard-emacs-ui)

;;; bard-emacs-ui.el ends here
