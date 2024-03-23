(require 'cl-lib)
(require 'eshell)

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

(global-set-key (kbd "C-z") nil)
(define-key global-map (kbd "C-z e") #'eshell-switcher)
(with-eval-after-load "esh-mode"
  (define-key eshell-mode-map (kbd "C-c f") #'bard/eshell-find-file-at-point)
  (define-key eshell-mode-map (kbd "C-c h") #'prot-eshell-narrow-output-highlight-regexp)
  (define-key eshell-mode-map (kbd "C-c d") #'prot-eshell-complete-recent-dir)
  (define-key eshell-mode-map (kbd "M-k") #'eshell-kill-input))

(provide 'bard-eshell.el)
