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
The exact color values are taken from the active Modus theme."
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

  (setq modus-themes-mixed-fonts t))

(use-package theme-buffet
  :ensure t
  :defer nil
  :bind
  (("<f6>" . theme-buffet-a-la-carte))
  :config
  (setq theme-buffet-menu 'end-user)
  (setq theme-buffet-end-user
        '(:night
          (ef-autumn ef-duo-dark ef-night ef-winter ef-dark)
          :twilight
          (ef-bio ef-cherie modus-vivendi)
          :morning
          (ef-elea-light ef-maris-light ef-spring)
          :day
          (ef-frost ef-light ef-trio-light modus-operandi)
          :afternoon
          (ef-cyprus ef-arbutus ef-day ef-duo-light ef-kassio ef-melissa-light ef-summer modus-operandi-tinted)
          :evening
          (ef-elea-dark ef-maris-dark ef-melissa-dark ef-symbiosis ef-trio-dark)))
  (theme-buffet-a-la-carte)
  (theme-buffet-timer-hours 1)
  (theme-buffet-mode t))

;;;; Fonts
(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((small
           :default-height 150
	       :default-family "Iosevka Comfy"
	       :variable-pitch-family "Iosevka Comfy Motion Duo"
	       :variable-pitch-height 1.0
	       :fixed-pitch-family "Iosevka Comfy"
	       :fixed-pitch-height 1.0
	       :bold-weight bold
	       :mode-line-active-family "Iosevka Comfy"
	       :mode-line-active-height 150
	       :mode-line-inactive-height 150
	       )
          (medium
	       :inherit small
	       :default-height 180
	       :mode-line-active-height 180
	       :mode-line-inactive-height 180)
          (large
	       :inherit small
	       :variable-pitch-family "Iosevka Comfy Wide Motion"
	       :default-height 200
	       :mode-line-active-height 180
	       :mode-line-inactive-height 180
	       )
	      (essay
           :inherit small
           :default-family "Monospace"
           :fixed-pitch-family "Monospace"
           :mode-line-active-family "Monospace"
           :variable-pitch-family "Helvetic Neue")
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



;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(use-package cursory
  :ensure t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 1.0)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 1.0)
          (bar-global
           :inherit bar
           :cursor-in-non-selected-windows t)
          (box-no-blink
           :blink-cursor-mode -1)
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
  :hook
  (kill-emacs . cursory-store-latest-preset)
  :bind
  (("C-c p" . cursory-set-preset)))

;; Switching themes
(defun bard/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
