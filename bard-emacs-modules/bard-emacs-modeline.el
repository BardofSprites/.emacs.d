;;; bard-emacs-modeline.el --- My customizations for the Emacs Modeline -*- lexical-binding: t -*-

;; Author: BardofSprites
;; Maintainer: BardofSprites
;; Version: 0.1.0
;; Package-Requires: ((Emacs 29.2))
;; Homepage: https://github.com/BardofSprites/.emacs.d
;; Keywords: emacs modeline custom


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

;; This file is meant to be used with Modus or Ef themes made by Protesilaos Starvou

;;; Code:

;; Face customization

;; (defun bard/modus-light-faces ()
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(mode-line ((t :background ,bg-ochre :foreground ,fg-main :box (:line-width 6 :color ,bg-ochre :style nil)))))
;;     (custom-set-faces
;;      `(mode-line-inactive ((t :background ,bg-mode-line-inactive :foreground ,fg-dim :box (:line-width 6 :color ,bg-mode-line-inactive :style nil)))))))

;; (defun bard/modus-dark-faces ()
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(mode-line ((t :background ,bg-magenta-subtle :foreground ,fg-main :box (:line-width 6 :color ,bg-magenta-subtle :style nil)))))
;;     (custom-set-faces
;;      `(mode-line-inactive ((t :background ,bg-mode-line-inactive :foreground ,fg-dim :box (:line-width 6 :color ,bg-mode-line-inactive :style nil)))))))

;; (defun bard/modus-themes-faces ()
;;   (if (or (memq 'modus-operandi custom-enabled-themes)
;;           (memq 'modus-operandi-tinted custom-enabled-themes))
;;       (bard/modus-light-faces)
;;     (bard/modus-dark-faces)
;;     ))

;; (modus-themes-with-colors
;;      (custom-set-faces
;;       `(mode-line ((t :background ,bg-purple-subtle :foreground ,fg-main :box (:line-width 3 :color ,purple-cooler)))))
;;      (custom-set-faces
;;       `(mode-line-inactive ((t :background ,bg-purple-nuanced :foreground ,fg-dim :box (:line-width 3 :color ,purple-faint))))))

;; (add-hook 'modus-themes-after-load-theme-hook #'bard/modus-themes-faces)

;;; Mode line
(setq mode-line-compact nil) ; Emacs 28
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e"
                prot-modeline-narrow
		" "
                prot-modeline-buffer-status
		" "
                prot-modeline-input-method
                prot-modeline-buffer-identification
                "  "
                prot-modeline-major-mode
                prot-modeline-process
                "  "
                prot-modeline-vc-branch
                "  "
                prot-modeline-flymake
                "  "
		prot-modeline-align-right
                prot-modeline-misc-info))

(with-eval-after-load 'spacious-padding
  (defun prot/modeline-spacious-indicators ()
    "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
    (if (bound-and-true-p spacious-padding-mode)
        (set-face-attribute 'prot-modeline-indicator-button nil :box t)
      (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified)))

  ;; Run it at startup and then afterwards whenever
  ;; `spacious-padding-mode' is toggled on/off.
  (prot/modeline-spacious-indicators)

  (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators))


(provide 'bard-emacs-modeline)

;;; bard-emacs-modeline.el ends here
