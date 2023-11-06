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
(defun bard/common-modes-hook ()
  "Commonly used modes, bundled in one hook."
  (display-line-numbers-mode 1)
  (hl-todo-mode 1))

(add-hook 'emacs-lisp-mode-hook 'bard/common-modes-hook)
(add-hook 'haskell-mode-hook 'bard/common-modes-hook)
(add-hook 'clojure-mode-hook 'bard/common-modes-hook)

;; CPP Mode
(defun bard/c++-mode-keybindings ()
  (define-key 'c++-mode-map (kbd "C-c C-c") 'compile))

(add-hook 'c++-mode-hook 'bard/c++-mode-keybindings)
(add-hook 'c++-mode 'bard/common-modes-hook)

;; Haskell
(setq exec-path (append exec-path '("/home/bard/.ghcup/bin/haskell-language-server-wrapper")))
(setq exec-path (append exec-path '("/home/bard/.ghcup/bin/haskell-language-server-9.0.2")))
(add-to-list 'exec-path "/home/bard/.ghcup/bin")

(provide 'bard-emacs-lang.el)
;;; bard-emacs-lang.el ends here
