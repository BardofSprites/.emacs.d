(defun bard/common-modes-hook ()
  "Commonly used modes, bundled in one hook"
  (display-line-numbers-mode 1)
  (electric-pair-mode 1)
  (hl-todo-mode 1))

(add-hook 'emacs-lisp-mode-hook 'bard/common-modes-hook)
(add-hook 'haskell-mode-hook 'bard/common-modes-hook)
(add-hook 'clojure-mode-hook 'bard/common-modes-hook)
