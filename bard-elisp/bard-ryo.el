(defun bard/ryo-insert-p ()
  "Return t if `ryo-modal-mode` is not bound or not active."
  (not (bound-and-true-p ryo-modal-mode)))

(provide 'bard-ryo)
