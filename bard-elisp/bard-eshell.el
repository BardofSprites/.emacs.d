(require 'cl-lib)
(require 'eshell)

;; aliases
(setq bard/eshell-aliases
      '((g  . magit)
        (gl . magit-log)
        (d  . dired)
        (o  . find-file)
        (oo . find-file-other-window)
        (l  . (lambda () (eshell/ls '-la)))
        (eshell/clear . eshell/clear-scrollback)))

(mapc (lambda (alias)
        (defalias (car alias) (cdr alias)))
      bard/eshell-aliases)

(defun prot-eshell--cd (dir)
  "Routine to cd into DIR."
  (delete-region eshell-last-output-end (point-max))
  (when (> eshell-last-output-end (point))
    (goto-char eshell-last-output-end))
  (insert-and-inherit "cd " (eshell-quote-argument dir))
  (eshell-send-input))

(defun prot-eshell-complete-recent-dir (dir &optional arg)
  "Switch to a recent Eshell directory.
When called interactively, DIR is selected with completion from
the elements of `eshell-last-dir-ring'.
With optional ARG prefix argument (\\[universal-argument]) also
open the directory in a `dired' buffer."
  (interactive
   (list
    (if-let ((dirs (ring-elements eshell-last-dir-ring)))
        (completing-read "Switch to recent dir: " dirs nil t)
      (user-error "There is no Eshell history for recent directories"))
    current-prefix-arg))
  (prot-eshell--cd dir)
  ;; UPDATE 2022-01-04 10:48 +0200: The idea for `dired-other-window'
  ;; was taken from Sean Whitton's `spw/eshell-cd-recent-dir'.  Check
  ;; Sean's dotfiles: <https://git.spwhitton.name/dotfiles>.
  (when arg
    (dired-other-window dir)))

(defun bard/eshell-find-file-at-point ()
  "Run `find-file` to find file"
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (find-file file)
      (user-error "No file at point"))))

(defcustom prot-eshell-output-buffer "*Exported Eshell output*"
  "Name of buffer with the last output of Eshell command.
Used by `prot-eshell-export'."
  :type 'string
  :group 'prot-eshell)

(defcustom prot-eshell-output-delimiter "* * *"
  "Delimiter for successive `prot-eshell-export' outputs.
This is formatted internally to have newline characters before
and after it."
  :type 'string
  :group 'prot-eshell)

(defun prot-eshell--command-prompt-output ()
  "Capture last command prompt and its output."
  (let ((beg (save-excursion
               (goto-char (eshell-beginning-of-input))
               (goto-char (point-at-bol)))))
    (when (derived-mode-p 'eshell-mode)
      (buffer-substring-no-properties beg (eshell-end-of-output)))))

;;;###autoload
(defun prot-eshell-export ()
  "Produce a buffer with output of the last Eshell command.
If `prot-eshell-output-buffer' does not exist, create it.  Else
append to it, while separating multiple outputs with
`prot-eshell-output-delimiter'."
  (interactive)
  (let ((eshell-output (prot-eshell--command-prompt-output)))
    (with-current-buffer (get-buffer-create prot-eshell-output-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (eq (point-min) (point-max))
          (insert (format "\n%s\n\n" prot-eshell-output-delimiter)))
        (goto-char (point-at-bol))
        (insert eshell-output)
        (switch-to-buffer-other-window (current-buffer))))))

(defgroup bard-eshell-faces nil
  "Faces for my custom modeline."
  :group 'prot-eshell-faces)

(defface bard-eshell-highlight-yellow-bg
  '((default :inherit (bold prot-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'bard-eshell-faces)

(defun prot-eshell-narrow-output-highlight-regexp (regexp)
  "Narrow to last command output and highlight REGEXP."
  (interactive
   (list (read-regexp "Regexp to highlight" nil 'prot-eshell--output-highlight-history)))
  (narrow-to-region (eshell-beginning-of-output)
                    (eshell-end-of-output))
  (goto-char (point-min))
  (highlight-regexp regexp 'prot-eshell-highlight-yellow-bg)
  (message "%s to last output and highlighted '%s'"
           (propertize "Narrowed" 'face 'bold)
           (propertize regexp 'face 'italic)))

(defun select-or-create (arg)
  "Commentary ARG."
  (if (string= arg "New eshell")
      (eshell t)
    (switch-to-buffer arg)))
(defun eshell-switcher (&optional arg)
  "Commentary ARG."
  (interactive)
  (let* (
         (buffers (cl-remove-if-not (lambda (n) (eq (buffer-local-value 'major-mode n) 'eshell-mode)) (buffer-list)) )
         (names (mapcar (lambda (n) (buffer-name n)) buffers))
         (num-buffers (length buffers) )
         (in-eshellp (eq major-mode 'eshell-mode)))
    (cond ((eq num-buffers 0) (eshell (or arg t)))
          ((not in-eshellp) (switch-to-buffer (car buffers)))
          (t (select-or-create (completing-read "Select Shell:" (cons "New eshell" names)))))))

;; taken from https://github.com/karthink/.emacs.d/blob/master/lisp/setup-shells.el
(use-package eshell
  :defer
  :config
  (setq eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'bard/eshell-default-prompt-fn)

  (defun bard/eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (concat (if (bobp) "" "\n")
            (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name pwd))
                          'face 'bard/eshell-prompt-pwd))
            (propertize (bard/eshell--current-git-branch)
                        'face 'bard/eshell-prompt-git-branch)
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  (defsubst bard/eshell--current-git-branch ()
    ;; TODO Refactor me
    (cl-destructuring-bind (status . output)
        (with-temp-buffer (cons
                           (or (call-process "git" nil t nil "symbolic-ref" "-q" "--short" "HEAD")
                               (call-process "git" nil t nil "describe" "--all" "--always" "HEAD")
                               -1)
                           (string-trim (buffer-string))))
      (if (equal status 0)
          (format " [%s]" output)
        "")))

  (defface bard/eshell-prompt-pwd '((t (:inherit font-lock-keyword-face)))
    "TODO"
    :group 'eshell)

  (defface bard/eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
    "TODO"
    :group 'eshell))

(provide 'bard-eshell)
