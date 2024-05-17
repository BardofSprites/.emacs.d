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

(use-package corfu
    :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay nil)
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package minibuffer
  :ensure nil
  :config
;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless))

  (setq completion-category-defaults nil)

  (setq completion-category-overridesd
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

(use-package consult
  :defer 2
  :bind*
  ("M-g M-g" . consult-goto-line)
  ("C-x b" . consult-buffer)
  ("M-s M-f" . consult-find)
  ("M-s M-g" . consult-grep)
  ("M-s M-h" . consult-history)
  ("M-s M-y" . consult-yank-pop)
  ("M-s M-o" . consult-outline)
  ("M-s M-l" . consult-line)
  :config
  (setq consult-find-args
        (concat "find . -not ( "
                "-path */.git* -prune "
                "-or -path */.cache* -prune )")))

(use-package embark
  :ensure t
  :bind
  (("C-," . bard-embark-act-no-quit)
   ("C-." . bard-embark-act-quit))
  :config

  (setq embark-keymap-alist
          '((buffer bard-embark-buffer-map)
            (command bard-embark-command-map)
            (expression bard-embark-expression-map)
            (file bard-embark-file-map)
            (function bard-embark-function-map)
            (identifier bard-embark-identifier-map)
            (package bard-embark-package-map)
            (region bard-embark-region-map)
            (symbol bard-embark-symbol-map)
            (url bard-embark-url-map)
            (variable bard-embark-variable-map)
            (t embark-general-map)))

  (defun bard-embark-act-no-quit ()
    "Call `embark-act' but do not quit after the action."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act)))

  (defun bard-embark-act-quit ()
    "Call `embark-act' and quit after the action."
    (interactive)
    (let ((embark-quit-after-action t))
      (call-interactively #'embark-act))
    (when (and (> (minibuffer-depth) 0)
               (derived-mode-p 'completion-list-mode))
      (abort-recursive-edit)))


  (setq embark-confirm-act-all nil)
  ;; The prot-embark.el has an advice to further simplify the
  ;; minimal indicator.  It shows cycling, which I never want to see
  ;; or do.
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.0)
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
  (setq embark-verbose-indicator-buffer-sections '(bindings))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-cycle embark-act-all embark-collect embark-export embark-insert))
    )

(use-package imenu-list
  :config
  (setq imenu-list-idle-update-delay 0.0)
  (setq org-imenu-depth 2)
  :bind
  (("C-`" . imenu-list-smart-toggle)))

;; Savehist
(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 100)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(register-alist kill-ring))
(savehist-mode 1)

;;; abbrev-mode
(setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
(setq only-global-abbrevs nil)

(bard/make-abbrev global-abbrev-table
  "meweb" "https://bardman.dev"
  "megit" "https://github.com/BardofSprites"
  "protweb" "https://protesilaos.com/")

(bard/make-abbrev text-mode-abbrev-table
    "asciidoc"       "AsciiDoc"
    "auctex"         "AUCTeX"
    "cafe"           "café"
    "cliche"         "cliché"
    "clojurescript"  "ClojureScript"
    "emacsconf"      "EmacsConf"
    "github"         "GitHub"
    "gitlab"         "GitLab"
    "javascript"     "JavaScript"
    "latex"          "LaTeX"
    "libreplanet"    "LibrePlanet"
    "linkedin"       "LinkedIn"
    "paypal"         "PayPal"
    "sourcehut"      "SourceHut"
    "texmacs"        "TeXmacs"
    "typescript"     "TypeScript"
    "visavis"        "vis-à-vis"
    "vscode"         "Visual Studio Code"
    "youtube"        "YouTube"
    "Результат"      "=Результат Сегодняшний Битвый="
    "asf"           "and so on and so forth")

(dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
  (add-hook hook #'abbrev-mode))

(remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

(provide 'bard-emacs-completion)
;;; bard-emacs-completion.el ends here
