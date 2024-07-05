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
  :ensure t)

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((small
           :default-height 130
	       :default-family "Iosevka Comfy"
	       :variable-pitch-family "Iosevka Comfy Motion"
	       :variable-pitch-height 1.0
	       :fixed-pitch-family "Iosevka Comfy"
	       :fixed-pitch-height 1.0
	       :bold-weight bold
	       :mode-line-active-family "Iosevka Comfy"
	       :mode-line-active-height 130
	       :mode-line-inactive-height 130
	       )
          (medium
	       :inherit small
	       :default-height 150
	       :mode-line-active-height 150
	       :mode-line-inactive-height 150)
          (large
	       :inherit small
	       :variable-pitch-family "Iosevka Comfy Wide Motion"
	       :default-height 170
	       :mode-line-active-height 150
	       :mode-line-inactive-height 150
	       )
	      (essay
           :inherit small
           :variable-pitch-family "Helvetica Neue")
          (t
           :default-family "Monospace"
           )))

  ;; save file
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'small))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; preserve fonts when switching themes
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
