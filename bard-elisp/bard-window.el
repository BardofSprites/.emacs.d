(defun prot-window-select-fit-size (window)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `prot-window-window-sizes'.

Use this as the `body-function' in a `display-buffer-alist' entry."
  (select-window window)
  (fit-window-to-buffer
   window
   (prot-window--get-window-size :max-height)
   (prot-window--get-window-size :min-height)
   (prot-window--get-window-size :max-width)
   (prot-window--get-window-size :min-width))
  ;; If we did not use `display-buffer-below-selected', then we must
  ;; be in a lateral window, which has more space.  Then we do not
  ;; want to dedicate the window to this buffer, because we will be
  ;; running out of space.
  (when (or (window-in-direction 'above) (window-in-direction 'below))
    (set-window-dedicated-p window t)))

(defun prot-window--get-display-buffer-below-or-pop ()
  "Return list of functions for `prot-window-display-buffer-below-or-pop'."
  (list
   #'display-buffer-reuse-mode-window
   (if (or (prot-common-window-small-p)
           (prot-common-three-or-more-windows-p))
       #'display-buffer-below-selected
     #'display-buffer-pop-up-window)))

(defun prot-window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`prot-common-window-small-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (prot-window--get-display-buffer-below-or-pop)))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(provide 'bard-window)
