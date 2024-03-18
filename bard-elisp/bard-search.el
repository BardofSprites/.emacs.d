;; Code provided by Protesilaos Starvou's dotemacs configuration.

;;;; Outline

(defun prot-search--get-outline ()
  "Return alist of outline outline-regexp and positions."
  (let* ((outline-regexp (format "^\\(?:%s\\)" (or (bound-and-true-p outline-regexp) "[*\^L]+")))
         (heading-alist (bound-and-true-p outline-heading-alist))
         (level-fun (or (bound-and-true-p outline-level)
                        (lambda () ;; as in the default from outline.el
                          (or (cdr (assoc (match-string 0) heading-alist))
                              (- (match-end 0) (match-beginning 0))))))
         candidates)
    (save-excursion
      (goto-char (point-min))
      (while (if (bound-and-true-p outline-search-function)
                 (funcall outline-search-function)
               (re-search-forward outline-regexp nil t))
        (push
         (format "%-5s %s"
                 (line-number-at-pos (point))
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         candidates)
        (goto-char (1+ (line-end-position)))))
    (if candidates
        (nreverse candidates)
      (user-error "No outline"))))

(defun prot-search--outline-prompt ()
  "Prompt for outline among headings retrieved by `prot-search--get-outline'."
  (completing-read
   "Go to outline: "
   (prot-common-completion-table-no-sort 'imenu (prot-search--get-outline))
   nil :require-match))

(defvar prot-search-outline-hook nil
  "Normal hook to run at the end of `prot-search-outline'.")

;;;###autoload
(defun prot-search-outline ()
  "Go to the line of the given outline using completion."
  (interactive)
  (when-let ((selection (prot-search--outline-prompt))
             (line (string-to-number (car (split-string selection "\t")))))
    (goto-line line)
    (run-hooks 'prot-search-outline-hook)))

(define-key global-map (kbd "M-s M-o") #'prot-search-outline)
