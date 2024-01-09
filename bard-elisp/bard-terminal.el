(defun open-terminal-in-current-directory ()
  "Open a terminal in the current working directory."
  (interactive)
  (let ((default-directory default-directory))
    (start-process "urxvt-terminal" nil "urxvtc")))

(global-set-key (kbd "C-c t") 'open-terminal-in-current-directory)

(provide 'bard-terminal.el)
