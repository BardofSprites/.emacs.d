(defun open-terminal-in-current-directory ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (start-process "st-terminal" nil "st")))

(global-set-key (kbd "C-c t") 'open-terminal-in-current-directory)

(provide 'bard-terminal.el)
