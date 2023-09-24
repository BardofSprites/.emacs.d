(require 'hs-lint)
(defun bard/haskell-mode-hook ()
    (local-set-key (kbd "C-c h l") 'hs-lint))
(add-hook 'haskell-mode-hook 'bard/haskell-mode-hook)
