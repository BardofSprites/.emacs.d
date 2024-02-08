(defun bard/open-terminal-in-current-directory ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (term "/bin/bash")))

(global-set-key (kbd "C-c t") 'bard/open-terminal-in-current-directory)

(defun bard/open-terminal-emulator ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (start-process "st terminal" nil "st")))

(with-eval-after-load "dired-mode"
  (define-key dired-mode-map (kbd "C-c C-t") 'bard/open-terminal-emulator))

(provide 'bard-terminal.el)
