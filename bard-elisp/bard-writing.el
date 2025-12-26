(require 'consult)
(require 'beframe)
(require 'calendar)
(require 'org-roam-node)
(require 'denote)

(defvar bard/consult--source-notes
  `(:name     "Note Buffers"
    :narrow   ?n
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :items    ,(lambda ()
                 (mapcar #'buffer-name
                         (seq-filter
                          (lambda (buf)
                            (string-prefix-p "[Note]" (buffer-name buf)))
                          (beframe-buffer-list))))
    :action   ,#'switch-to-buffer
    :state    ,#'consult--buffer-state)
  "Consult source for note buffers (limited to beframe buffers).")


(defun bard/find-notes-file ()
  (interactive)
  (consult-find "~/Notes/denote"))

(defun bard/search-notes-directory ()
  (interactive)
  (consult-grep "~/Notes/denote"))

(defun bard/consult-buffer-notes ()
  "Show `consult-buffer` limited to buffers starting with [Note]."
  (interactive)
  (consult-buffer '(bard/consult--source-notes)))

(defun bard/ibuffer-notes ()
  "Open `ibuffer` limited to buffers starting with [Note]."
  (interactive)
  (ibuffer nil "*Ibuffer-Notes*"
           '((name . "^\\[Note\\]"))))

(defun denote-sequence-region ()
  "Call `denote-sequence' and insert therein the text of the active region.

Note that, currently, `denote-save-buffers' and
`denote-kill-buffers' are NOT respected.  The buffer is not
saved or killed at the end of `denote-sequence-region'."
  (declare (interactive-only t))
  (interactive)
  (if-let* (((region-active-p))
            ;; We capture the text early, otherwise it will be empty
            ;; the moment `insert' is called.
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      (progn
        (let ((denote-ignore-region-in-denote-command t)
              ;; FIXME: Find a way to insert the region before the buffer is
              ;; saved/killed by the creation command.
              (denote-save-buffers nil)
              (denote-kill-buffers nil))
          (call-interactively 'denote-sequence))
        (push-mark (point))
        (insert text)
        (run-hook-with-args 'denote-region-after-new-note-functions (mark) (point)))
    (call-interactively 'denote-sequence)))

(defvar bard/class-dirs
  '(("ANTH 204" . "~/Documents/dox/Uni/FALL2025-ANTH 204/")
    ("CHEM 201" . "~/Documents/dox/Uni/FALL2025-CHEM 201/")
    ("CHEM 207" . "~/Documents/dox/Uni/FALL2025-CHEM 207/")
    ("ENGL 105" . "~/Documents/dox/Uni/FALL2025-ENGL 105/")
    ("ENGR 101" . "~/Documents/dox/Uni/FALL2025-ENGR 101/")
    ("ENGR 110" . "~/Documents/dox/Uni/FALL2025-ENGR 110/"))
  "Mapping of class names to their document directories.")

(defvar bard/uni-notes-file "~/Notes/denote/uni.org"
  "Path to the main university org file.")

(defun bard/jump-to-class (class)
  "Jump to CLASS heading in `bard/uni-notes-file` and open its dir in dired."
  (interactive
   (list (completing-read "Class: " (mapcar #'car bard/class-dirs))))
  (let* ((dir (cdr (assoc class bard/class-dirs))))
    ;; split windows
    (delete-other-windows)
    (let ((notes-window (selected-window))
          (dired-window (split-window-right)))
      ;; open notes file and jump to heading
      (with-selected-window notes-window
        (find-file bard/uni-notes-file)
        (widen)
        (goto-char (point-min))
        (message class)
        (search-forward class nil nil))
      ;; open dired in right window
      (with-selected-window dired-window
        (dired dir)))))

;; (defun bard/jump-to-class-new-frame (class)
;;   "Open CLASS notes and dir in a new frame titled after CLASS."
;;   (interactive
;;    (list (completing-read "Class: " (mapcar #'car bard/class-dirs))))
;;   (let* ((dir (cdr (assoc class bard/class-dirs)))
;;          ;; make a new frame with title
;;          (frame (make-frame `((name . ,class)))))
;;     (select-frame-set-input-focus frame)
;;     (delete-other-windows)
;;     (let ((notes-window (selected-window))
;;           (dired-window (split-window-right)))
;;       ;; open notes file and jump to heading
;;       (with-selected-window notes-window
;;         (find-file bard/uni-notes-file)
;;         (widen)
;;         (goto-char (point-min))
;;         (search-forward class nil nil))
;;       ;; open dired in right window
;;       (with-selected-window dired-window
;;         (dired dir)))))

(defun bard/jump-to-class-new-frame (class)
  "Open CLASS notes and dir in a new frame titled after CLASS, even with beframe."
  (interactive
   (list (completing-read "Class: " (mapcar #'car bard/class-dirs))))
  (let* ((dir (cdr (assoc class bard/class-dirs)))
         (frame (make-frame `((frame-title-format . ,class)))))
    (select-frame-set-input-focus frame)
    (delete-other-windows)
    (let ((notes-window (selected-window))
          (dired-window (split-window-right)))
      (with-selected-window notes-window
        (find-file bard/uni-notes-file)
        (widen)
        (goto-char (point-min))
        (search-forward class nil nil))
      (with-selected-window dired-window
        (dired dir)
        (beframe-rename-current-frame)))))

;; Optional: bind to a key
(global-set-key (kbd "C-c u") #'bard/jump-to-class)
(global-set-key (kbd "C-c U") #'bard/jump-to-class-new-frame)

(defun bard/denote-todo-template ()
  "Return string for daily tasks heading in `denote-journal' entries"
  (format "* Tasks for %s\n\n* Notes for today"
          (format-time-string "%Y-%m-%d (%a)")))

;; Taken from: https://stackoverflow.com/a/75314192
(defun add-multiple-into-list (lst items)
    "Add each item from ITEMS into LST."
    (dolist (item items)
        (add-to-list lst item)))

(defun bard/cdlatex-add-math-symbols ()
  "Add functions into list."
  (add-multiple-into-list
   'cdlatex-math-symbol-alist-comb
   '((?V "\\vec"))))

(define-minor-mode bard/org-math-mode
  "Enable features to write math in `org-mode'."
  :init-value nil
  :lighter " S="
  :global nil
  (org-fragtog-mode t)
  (org-cdlatex-mode t)
  (electric-pair-local-mode t)
  (bard/cdlatex-add-math-symbols))


(provide 'bard-writing)
