;;; bard-emacs-completion.el --- My configurations for emacs completion frameworks -*- lexical-binding: t -*-

;; Author: bard
;; Maintainer: bard
;; Version: 0.1.0
;; Package-Requires: ((emacs 29.2))
;; Homepage: github
;; Keywords: completion


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

;; minibuffer completion
(use-package vertico
  :init
  (vertico-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package company
  :init
  (global-company-mode 1))

(use-package consult
  :defer 2
  :bind
  ("C-x b" . consult-buffer)
  ("C-s" . consult-line)
  ("C-S-s" . consult-line-multi)
  ("C-z s" . consult-ripgrep))

;; (use-package embark
;;   :ensure t
;;   :config
;;   (setq embark-confirm-act-all nil)
;;     ;; The prot-embark.el has an advice to further simplify the
;;     ;; minimal indicator.  It shows cycling, which I never want to see
;;     ;; or do.
;;     (setq embark-mixed-indicator-both nil)
;;     (setq embark-mixed-indicator-delay 1.0)
;;     (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
;;     (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
;;     (setq embark-verbose-indicator-buffer-sections '(bindings))
;;     (setq embark-verbose-indicator-excluded-actions
;;           '(embark-cycle embark-act-all embark-collect embark-export embark-insert))

;;     )

(use-package imenu-list
  :bind
  (("C-`" . imenu-list-smart-toggle)))

;; Savehist
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 100)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(register-alist kill-ring))
(savehist-mode 1)

(provide 'bard-emacs-completion)
;;; bard-emacs-completion.el ends here
