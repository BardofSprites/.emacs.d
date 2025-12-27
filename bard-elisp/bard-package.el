;; taken and renamed functions from 

(defun bard/package-report ()
  "Report total package counts grouped by archive."
  (interactive)
  (package-refresh-contents)
  (bard/display-package-report
   (let* ((arch-pkgs (bard/archive-packages))
          (counts (seq-sort-by #'cdr #'> (bard/archive-counts arch-pkgs)))
          (by-arch (seq-group-by #'car arch-pkgs)))
     (concat
      (format "Total packages: %s\n\n" (apply #'+ (mapcar #'cdr counts)))
      (mapconcat
       (lambda (archive)
         (concat "• "
                 (format "%s (%s)" (car archive) (cdr archive))
                 ": "
                 (mapconcat (lambda (ap-pair) (cdr ap-pair))
                            (alist-get (car archive) by-arch)
                            ", ")))
       counts
       "\n\n")))))

(defun bard/display-package-report (output)
  "Display OUTPUT in a popup buffer."
  (let ((buffer-name "*package-report*"))
    (with-help-window buffer-name
      (with-current-buffer buffer-name
        (visual-line-mode 1)
        (erase-buffer)
        (insert output)
        (goto-char (point-min))))))

(defun bard/archive-packages ()
  "Return a list of (archive . package) cons cells."
  (seq-reduce
   (lambda (res package)
     (let ((archive (package-desc-archive
                     (cadr (assq package package-archive-contents))))
           (pkg (symbol-name package)))
       (push (cons archive pkg) res)))
   (mapcar #'car package-alist)
   nil))

(defun bard/archive-counts (arch-pkgs)
  "Return a list of cons cells from alist ARCH-PKGS.
The cars are package archives, the cdrs are the number of
packages installed from each archive."
  (seq-reduce
   (lambda (counts key)
     (cons (cons key (+ 1 (or (cdr (assoc key counts)) 0)))
           (assoc-delete-all key counts)))
   (mapcar #'car arch-pkgs)
   nil))

(provide 'bard-package)
