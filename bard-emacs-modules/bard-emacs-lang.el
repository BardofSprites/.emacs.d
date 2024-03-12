;;; bard-emacs-lang.el --- configuration for specific languages -*- lexical-binding: t -*-

;; Author: Bardman
;; Maintainer: Bardman
;; Version: 0.1.0
;; Package-Requires: ((Emacs "28.1"))

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

;; Input methods
(global-set-key (kbd "<f10>") 'toggle-input-method)

;; Snippet directories
(setq yasnippet-snippets-dir '("/home/bard/.emacs.d/elpa/yasnippet-snippets-20231219.1505/snippets" "/home/bard/.emacs.d/snippets/"))

(defun bard/common-modes-hook ()
  "Commonly used modes, bundled in one hook."
  (display-line-numbers-mode 1)
  (bard/set-up-whitespace-handling)
  (hl-todo-mode 1))

(add-hook 'prog-mode-hook 'bard/common-modes-hook)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)

;; CPP Mode
;; (defun bard/c++-mode-keybindings ()
;;   (define-key 'c++-mode-map (kbd "C-c C-c") 'compile))

;; (add-hook 'c++-mode-hook 'bard/c++-mode-keybindings)
(add-hook 'c++-mode 'bard/common-modes-hook)
(setq-default c-basic-offset 4)
(with-eval-after-load "c++-mode"
  (define-key c++-mode-map (kbd "C-z s") #'consult-ripgrep))

;; Haskell
;; (add-to-list 'company-backends 'company-dabbrev-code)
;; (add-to-list 'company-backends 'company-yasnippet)
;; (add-to-list 'company-backends 'company-files)

;; disable ghci popups
(setq haskell-interactive-popup-errors nil)

(add-to-list 'exec-path "/home/bard/.ghcup/bin")
(add-to-list 'exec-path "/home/bard/.cabal/bin")
(add-to-list 'exec-path "/home/bard/.local/bin")
(add-to-list 'exec-path "/home/bard/opt/")

;; GGTAGS
(with-eval-after-load 'ggtags
  (define-key ggtags-mode-map (kbd "M->") nil)
  (define-key ggtags-mode-map (kbd "M-<") nil))

(provide 'bard-emacs-lang.el)
;;; bard-emacs-lang.el ends here
