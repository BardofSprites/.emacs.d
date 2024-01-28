(defun bard/open-terminal-in-current-directory ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (term "/bin/bash")))

(global-set-key (kbd "C-c t") 'bard/open-terminal-in-current-directory)

(provide 'bard-terminal.el)
