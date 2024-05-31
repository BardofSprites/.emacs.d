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

;; commentary

;;; Code:

;; declare all themes as safe (i trust developers)
(setq custom-safe-themes t)

;; Ef-themes
(use-package ef-themes
  :ensure t
  :hook
  ((ef-themes-post-load . bard/ef-themes-hl-todo-faces)
   (ef-themes-post-load . logos-update-fringe-in-buffers))
  :config
  (defun bard/ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
	        `(("WAIT" . ,yellow)
	          ("TODO" . ,red)
	          ("NEXT" . ,blue)
	          ("THEM" . ,magenta)
	          ("PROG" . ,cyan-warmer)
	          ("OKAY" . ,green-warmer)
	          ("DONT" . ,yellow-warmer)
	          ("FAIL" . ,red-warmer)
	          ("BUG" . ,red-warmer)
	          ("DONE" . ,green)
	          ("NOTE" . ,blue-warmer)
	          ("KLUDGE" . ,cyan)
	          ("HACK" . ,cyan)
	          ("TEMP" . ,red)
	          ("FIXME" . ,red-warmer)
	          ("XXX+" . ,red-warmer)
	          ("KILLED" . ,cyan)
	          ("REVIEW" . ,red)
	          ("DEPRECATED" . ,yellow)))))

  (setq ef-themes-headings
	    '((0 variable-pitch light 2.00)
	      (1 variable-pitch light 1.5)
	      (2 regular 1.3)
	      (3 1.1)
          (agenda-date . (variable-pitch regular 1.3))
	      (agenda-structure . (variable-pitch light 1.9))
	      (t . 1.1)))

  ;; verbatim need higher contrast for org mode
  (setq ef-elea-dark-palette-overrides
	    '((prose-verbatim yellow-cooler)))

  (setq ef-elea-light-palette-overrides
	    '((prose-verbatim yellow-cooler)))

  (define-key global-map (kbd "M-<f5>") #'ef-themes-select)
  (setq ef-themes-to-toggle '(ef-winter ef-frost))
  (setq ef-themes-mixed-fonts t))

(use-package modus-themes
  :ensure t
  :bind
  (("<f5>" . modus-themes-select))
  :hook ((modus-themes-post-load . bard/modus-themes-hl-todo-faces)
         (modus-themes-post-load . logos-update-fringe-in-buffers))
  :config
  (defun bard/modus-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Modus themes colors.
The exact color values are taken from the active Ef theme."
    (modus-themes-with-colors
      (setq hl-todo-keyword-faces
	        `(("WAIT" . ,yellow)
	          ("TODO" . ,red)
	          ("NEXT" . ,blue)
	          ("THEM" . ,magenta)
	          ("PROG" . ,cyan-warmer)
	          ("OKAY" . ,green-warmer)
	          ("DONT" . ,yellow-warmer)
	          ("FAIL" . ,red-warmer)
	          ("BUG" . ,red-warmer)
	          ("DONE" . ,green)
	          ("NOTE" . ,blue-warmer)
	          ("KLUDGE" . ,cyan)
	          ("HACK" . ,cyan)
	          ("TEMP" . ,red)
	          ("FIXME" . ,red-warmer)
	          ("XXX+" . ,red-warmer)
	          ("REVIEW" . ,red)
	          ("KILLED" . ,cyan)
	          ("DEPRECATED" . ,yellow)))))
  (setq modus-themes-headings
	    '((0 variable-pitch light 2.00)
	      (1 variable-pitch light 1.5)
	      (2 regular 1.3)
	      (3 1.1)
          (agenda-date . (variable-pitch regular 1.3))
	      (agenda-structure . (variable-pitch light 1.9))
	      (t . 1.1)))

  ;; org customization
  (setq org-priority-faces
        '((?A . (:inherit (bold org-priority)))
          (?B . org-priority)
          (?C . (:inherit (shadow org-priority)))))

  (setq modus-themes-mixed-fonts t)
  (load-theme 'modus-vivendi t))

(use-package theme-buffet
  :ensure t
  :bind
  (("<f6>" . theme-buffet-a-la-carte))
  :config
  (setq theme-buffet-menu 'end-user)
  (setq theme-buffet--end-user
        '( :night (modus-vivendi ef-autumn ef-winter ef-dream ef-symbiosis)
           :morning (modus-operandi ef-cyprus ef-spring ef-frost)
           :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-dark)
           :evening (modus-vivendi ef-elea-dark ef-melissa-dark ef-night ef-winter)))
  (theme-buffet-timer-hours 1))

;;;; Fonts
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
	       :default-height 140
	       :mode-line-active-height 140
	       :mode-line-inactive-height 140)
	      (large
	       :inherit small
	       :variable-pitch-family "Iosevka Comfy Wide Motion"
	       :default-height 170
	       :mode-line-active-height 140
	       :mode-line-inactive-height 140
	       )
	      (presentation
	       :inherit small
	       :default-height 170
	       :default-family "Iosevka Comfy Wide"
	       :variable-pitch-family "Iosevka Comfy Wide Motion"
	       :default-height 170
	       :mode-line-active-height 150
	       :mode-line-inactive-height 150)
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

;; Switching themes
(defun bard/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
