(require 'embark)

(defvar-keymap bard-embark-general-map
  :parent embark-general-map
  "i" #'embark-insert
  "w" #'embark-copy-as-kill
  "E" #'embark-export
  "S" #'embark-collect
  "A" #'embark-act-all
  "DEL" #'delete-region)

(defvar-keymap bard-embark-url-map
  :parent embark-general-map
  "b" #'browse-url
  "d" #'embark-download-url
  "e" #'eww)

(defvar-keymap bard-embark-buffer-map
  :parent embark-general-map
  "k" #'bard-simple-kill-buffer
  "o" #'switch-to-buffer-other-window
  "e" #'ediff-buffers)

(add-to-list 'embark-post-action-hooks (list 'bard-simple-kill-buffer 'embark--restart))

(defvar-keymap bard-embark-file-map
  :parent embark-general-map
  "f" #'find-file
  "j" #'embark-dired-jump
  "c" #'copy-file
  "e" #'ediff-files)

(defvar-keymap bard-embark-identifier-map
  :parent embark-general-map
  "h" #'display-local-help
  "." #'xref-find-definitions
  "o" #'occur)

(defvar-keymap bard-embark-command-map
  :parent embark-general-map
  "h" #'describe-command
  "." #'embark-find-definition)

(defvar-keymap bard-embark-expression-map
  :parent embark-general-map
  "e" #'pp-eval-expression
  "m" #'pp-macroexpand-expression)

(defvar-keymap bard-embark-function-map
  :parent embark-general-map
  "h" #'describe-function
  "." #'embark-find-definition)

(defvar-keymap bard-embark-package-map
  :parent embark-general-map
  "h" #'describe-package
  "i" #'package-install
  "d" #'package-delete
  "r" #'package-reinstall
  "b" #'embark-browse-package-url
  "w" #'embark-save-package-url)

(defvar-keymap bard-embark-symbol-map
  :parent embark-general-map
  "h" #'describe-symbol
  "." #'embark-find-definition)

(defvar-keymap bard-embark-variable-map
  :parent embark-general-map
  "h" #'describe-variable
  "." #'embark-find-definition)

(defvar-keymap bard-embark-region-map
  :parent embark-general-map
  "a" #'align-regexp
  "D" #'delete-duplicate-lines
  "f" #'flush-lines
  "i" #'epa-import-keys-region
  "d" #'epa-decrypt-armor-in-region
  "r" #'repunctuate-sentences
  "s" #'sort-lines
  "u" #'untabify)

;; The minimal indicator shows cycling options, but I have no use
;; for those.  I want it to be silent.
(defun bard-embark-no-minimal-indicator ())
(advice-add #'embark-minimal-indicator :override #'bard-embark-no-minimal-indicator)

(provide 'bard-embark)
