3;;; bard-emacs-modeline.el --- My customizations for the Emacs Modeline -*- lexical-binding: t -*-

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


(defun bard/modus-themes-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     `(mode-line ((t :background ,bg-magenta-subtle :foreground ,fg-main :box ,border))))
    (custom-set-faces
     `(mode-line-inactive ((t :background ,bg-magenta-nuanced :foreground ,fg-dim :box ,border))))))

(add-hook 'modus-themes-post-load-hook #'bard/modus-themes-faces)

;;; Mode line
(setq mode-line-compact nil) ; Emacs 28
(setq mode-line-right-align-edge 'right-margin) ; Emacs 30
(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                prot-modeline-narrow
                prot-modeline-buffer-status
                prot-modeline-input-method
                prot-modeline-evil
                prot-modeline-buffer-identification
                "  "
                prot-modeline-major-mode
                prot-modeline-process
                "  "
                prot-modeline-vc-branch
                "  "
                prot-modeline-eglot
                "  "
                prot-modeline-flymake
                "  "
                mode-line-format-right-align ; Emacs 30
                prot-modeline-notmuch-indicator
                "  "
                prot-modeline-misc-info))

(provide 'bard-emacs-modeline)

;;; bard-emacs-modeline.el ends here
