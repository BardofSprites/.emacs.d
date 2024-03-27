;; Text Scratch buffers
(defun bard/new-org-buffer ()
  (interactive)
  (let ((xbuf (generate-new-buffer "*org*")))
    (switch-to-buffer xbuf)
    (funcall (quote org-mode))
    (text-scale-increase 1.5)
    xbuf))
(define-key global-map (kbd "M-=") #'bard/new-org-buffer)

(defun bard/new-plain-buffer ()
  (interactive)
  (let ((xbuf (generate-new-buffer "*plain*")))
    (switch-to-buffer xbuf)
    (text-scale-increase 1.5)
    xbuf))

(define-key global-map (kbd "M--") #'bard/new-plain-buffer)

;; elisp scratch buffer

(defun bard/new-elisp-buffer ()
  (interactive)
  (let ((xbuf (generate-new-buffer "*elisp*")))
    (switch-to-buffer xbuf)
    (funcall (quote emacs-lisp-mode))
    (text-scale-increase 1.5)
    xbuf))

(define-key global-map (kbd "C-z C-s") #'bard/new-elisp-buffer)

(provide 'bard-scratch.el)
