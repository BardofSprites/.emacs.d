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
  :demand t
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

  (setq org-priority-faces
        '((?A . (:inherit (bold next-error)))
          (?B . org-priority)
          (?C . (:inherit (shadow org-priority)))))

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
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t)

  )

(use-package modus-themes
  :ensure t
  :demand t
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

  (setq modus-themes-custom-auto-reload nil
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold))
  (setq modus-themes-common-palette-overrides
        `((fringe unspecified)
          (bg-line-number-active bg-hl-line)
          (bg-line-number-inactive unspecified)
          (fg-line-number-active fg-main)
          ))
  ;; org customization
  (setq org-priority-faces
        '((?A . (:inherit (bold next-error)))
          (?B . org-priority)
          (?C . (:inherit (shadow org-priority)))))

  (load-theme 'modus-vivendi t))

;;;; Fonts
(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((default
           :default-height 160
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion"
           :variable-pitch-height 1.0
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :bold-weight bold
           :mode-line-active-family "Iosevka Comfy Motion"
           :mode-line-inactive-family "Iosevka Comfy Motion"
           :mode-line-active-height 160
           :mode-line-inactive-height 160
           )
          (wide
           :inherit default
           :default-height 150
           :default-family "Iosevka Comfy Wide"
           :fixed-pitch-family "Iosevka Comfy Wide"
           :mode-line-active-family "Iosevka Comfy Wide Motion Duo"
           :mode-line-inactive-family "Iosevka Comfy Wide Motion Duo"
           :variable-pitch-family "Iosevka Comfy Wide Motion Duo"
           :mode-line-active-height 150
           :mode-line-inactive-height 150)
          (large
           :inherit default
           :variable-pitch-family "Iosevka Comfy Wide Motion"
           :default-height 170
           :mode-line-active-height 150
           :mode-line-inactive-height 150
           )
          (huge
           :inherit laptop
           :variable-pitch-family "Iosevka Comfy Wide Motion"
           :default-height 200
           :mode-line-active-height 170
           :mode-line-inactive-height 170
           )
          (comic
           :inherit default
           :default-family "Comic Code"
           :variable-pitch-family "Comic Code"
           :fixed-pitch-family "Comic Code"
           :mode-line-active-family "Comic Code"
           )
          (berkeley
           :inherit desktop
           :default-height 150
           :mode-line-active-height 150
           :mode-line-inactive-height 150
           :default-family "Berkeley Mono"
           :variable-pitch-family "Berkeley Mono"
           :fixed-pitch-family "Berkeley Mono"
           :mode-line-active-family "Berkeley Mono"
           )
          (t
           :default-family "Monospace"
           )))

  ;; save file
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'laptop))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; preserve fonts when switching themes
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset))

(use-package rainbow-mode
  :ensure t)

;; Switching themes
(defun bard/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(provide 'bard-emacs-theme)

;;; bard-emacs-theme.el ends here
