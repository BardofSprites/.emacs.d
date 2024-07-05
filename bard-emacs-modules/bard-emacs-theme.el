;;; bard-emacs-theme.el --- My theme customizations -*- lexical-binding: t -*-

;; Author: Daniel Pinkston
;; Maintainer: Daniel Pinkston
;; Version: 0.1.0
;; Package-Requires: ((emacs 29.2))
;; Homepage: https://github.com/BardofSprites/.emacs.d
;; Keywords: theme fonts emacs


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

;; declare all themes as safe (i trust developers)
(setq custom-safe-themes t)

;;;; Fonts
(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(use-package emacs
  :ensure t
  :config
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 140)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy" :height 140)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Motion" :height 140))

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
