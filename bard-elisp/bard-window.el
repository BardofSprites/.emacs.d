(defvar prot-window-window-sizes
  '( :max-height (lambda () (floor (frame-height) 3))
     :min-height 10
     :max-width (lambda () (floor (frame-width) 4))
     :min-width 20)
  "Property list of maximum and minimum window sizes.
The property keys are `:max-height', `:min-height', `:max-width',
and `:min-width'.  They all accept a value of either a
number (integer or floating point) or a function.")

(defun prot-window--get-window-size (key)
  "Extract the value of KEY from `prot-window-window-sizes'."
  (when-let ((value (plist-get prot-window-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

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

;; from protesilaos prot-shell library
(defun prot-window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\|terminal\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      ;; REVIEW 2022-07-14: Is this robust?
      (and (not (derived-mode-p 'message-mode 'text-mode))
           (derived-mode-p 'eshell-mode 'shell-mode 'term-mode 'comint-mode 'fundamental-mode)))))

;; taken from https://github.com/hylophile/.files/blob/1f3f01e4e25b00f7b61eca286fcf4f865885090c/.config/doom/config.org#fancy-tab-bar

(defun hy/tab-bar-format-align-center ()
  "Align the rest of tab bar items centered."
  (let* ((rest (cdr (memq 'hy/tab-bar-format-align-center tab-bar-format)))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest  ""))
         (hpos (progn
                 (add-face-text-property 0 (length rest) 'tab-bar t rest)
                 (string-pixel-width rest)))
         (hpos (+ hpos (/ (- (frame-inner-width) hpos) 2)))
         (str (propertize "​" 'display
                          ;; The `right' spec doesn't work on TTY frames
                          ;; when windows are split horizontally (bug#59620)
                          (if (window-system)
                              `(space :align-to (- right (,hpos)))
                            `(space :align-to (,(- (frame-inner-width) hpos)))))))
    `((align-center menu-item ,str ignore))))

(setq tab-bar-tab-name-format-function #'hy/tab-bar-tab-name-format-default)
(defun hy/tab-bar-tab-name-format-default (tab i)
  (let* ((hint (format "%d" i))
         (name (alist-get 'name tab))
         (dir (concat "(" (alist-get 'dir tab "") ")"))
         (name-format (concat
                       " "
                       (propertize hint 'face 'tab-bar-hint)
                       " "
                       name
                       " ")))
    (add-face-text-property
     0 (length name-format)
     (funcall tab-bar-tab-face-function tab)
     'append name-format)
    name-format))


(setq tab-bar-tab-name-function #'hy/tab-bar-tab-name-current)
(defun hy/tab-bar-tab-name-current ()
  (hy/shorten-string
   (hy/abbreviate-tab-name
    (buffer-name (window-buffer (or (minibuffer-selected-window)
                                    (and (window-minibuffer-p)
                                         (get-mru-window))))))
   25))

(defun hy/set-tab-dir ()
  (setf (alist-get 'dir (cdr (tab-bar--current-tab-find)))
        (hy/tab-bar-dir)))

(defun hy/abbreviate-directory-path (path)
  "Turns `~/code/test/t` into `~/c/t/project`."
  (let* ((directories (seq-filter (lambda (s) (not (string= s ""))) (split-string path "/")))
         (last-dir (car (last directories)))
         (abbreviated-dirs (mapcar (lambda (dir)
                                     (if (string= dir last-dir)
                                         dir
                                       (substring dir 0 (if (string-prefix-p "." dir) 2 1))))
                                   directories)))
    (mapconcat 'identity abbreviated-dirs "/")))

(defun hy/tab-bar-dir ()
  (hy/shorten-string (hy/abbreviate-directory-path
                      (abbreviate-file-name
                       (or (projectile-project-root) default-directory)))
                     10
                     t))

(defun hy/shorten-string (string max-length &optional at-start)
  (let ((len (length string)))
    (if (> len max-length)
        (if at-start
            (concat  "…" (substring string (- len max-length) len))
          (concat (substring string 0 max-length) "…"))
      string)))

(defun hy/abbreviate-tab-name (name)
  (string-trim (replace-regexp-in-string
                (rx (or "*" "helpful" "Org Src"))
                "" name)))

(defun bard/toggle-window-split ()
  "Toggle between horizontal and vertical window splits, preserving buffer layout."
  (interactive)
  (let ((current-buffers (mapcar #'window-buffer (window-list))) ; List of buffers in current windows
        (split-direction (if (= (window-width) (frame-width))
                             'vertical
                           'horizontal)))
    (delete-other-windows)
    ;; Toggle the split direction
    (if (eq split-direction 'horizontal)
        (split-window-vertically)
      (split-window-horizontally))
    ;; Restore buffers to the new windows
    (let ((windows (window-list)))
      (cl-loop for buffer in current-buffers
               for window in windows
               do (set-window-buffer window buffer)))))

;; The ultimate DWIM other window command.
(defvar pmx--direction -1)
(defvar pmx--last-win nil)
(defun pmx-other-window (frame)
  "Switch window, with DWIM behavior.
Prefix argument FRAME will unconditionally switch frames.
When called without any windows to switch to, split and select.
If called not in repeat, reverse directions and switch back to
usually the most recent window (though not `get-mru-window').
Finally, when called in repeat, continue in the same direction so
that we can usually get to the right window faster than an `avy'
call unless there's a ton of windows for some reason."
  (interactive "P")
  (cond (frame (other-frame 1))          ; unconditional with prefix arg
        ((equal 1 (length (window-list
                           (selected-frame))))
         ;; If there is no window or even minibuffer open, split window.  Change
         ;; the direction so that we go back to the source window on repeat or
         ;; next call.
         (let ((source (selected-window))
               (tall (> (frame-pixel-height) (frame-pixel-width))))
           (select-window (split-window-right))
           (if (eq source (next-window))
               (setq pmx--direction 1)
             (setq pmx--direction -1)
             (when (not (eq source (previous-window)))
               (warn "Split window sucessor inconsistent")))))
        ((not (eq last-command 'pmx-other-window))
         ;; If we are not repeating an other-window command, reverse the
         ;; direction and select in that direction.
         (if (eq pmx--last-win (selected-window))
             (setq pmx--directionapmx--direction (- pmx--direction))
           ;;  we changed windows out of band. Reverse directions.
           (setq pmx--direction -1))
         (other-window pmx--direction))
        (t
         ;; We are repeating.  Continue going in the established direction.
         (other-window pmx--direction)))
  (setq pmx--last-win (selected-window)))

(keymap-global-set "C-x o" #'pmx-other-window)

(provide 'bard-window)
