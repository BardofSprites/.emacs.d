(require 'cl-lib)
(require 'eshell)

(defun bard/eshell-complete-recent-dir (&optional args)
  "Switch to a recent `eshell` directory using completion."
  (interactive "P")
  (let* ((dirs (ring-elements eshell-last-dir-ring))
	  (dir (vertico--exhibit ()
				 (completing-read "Switch to recent dir: " dirs nil t))))
       (insert-dir)
       (eshell-send-input)
       (when arg
	 (dired-dir))))

(defun bard/eshell-find-file-at-point ()
  "Run `find-file` to find file"
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
	(find-file file)
      (user-error "No file at point"))))

;; (defun bard/eshell-narrow-output-highlight-regexp ()
;;   (interactive)
;;   (let ((regexp (read-regexp "Regexp to highlight: ")))
;;     (narrow-to-region (eshell-beginning-of-output)
;; 		      (eshell-end-of-output))
;;     (goto-char (point-min))
;;     (highlight-regexp regexp 'hi-yellow)))


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

(with-eval-after-load "esh-mode"
  (define-key eshell-mode-map (kbd "C-c f") #'bard/eshell-find-file-at-point)
  (define-key eshell-mode-map (kbd "C-c h") #'bard/eshell-narrow-ouput-highlight-regexp)
  (define-key eshell-mode-map (kbd "C-c d") #'bard/eshell-complete-recent-dir)
  (define-key eshell-mode-map (kbd "M-k") #'eshell-kill-input)
  (global-set-key (kbd "C-z") nil)
  (define-key global-map (kbd "C-z e") #'eshell-switcher))
