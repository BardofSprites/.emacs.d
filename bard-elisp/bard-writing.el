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

(defun bard/consult-buffer-notes ()
  "Show `consult-buffer` limited to buffers starting with [Note]."
  (interactive)
  (consult-buffer '(bard/consult--source-notes)))

(defun bard/ibuffer-notes ()
  "Open `ibuffer` limited to buffers starting with [Note]."
  (interactive)
  (ibuffer nil "*Ibuffer-Notes*"
           '((name . "^\\[Note\\]"))))

(defun bard/find-notes-file ()
  (interactive)
  (consult-find "~/Notes/denote"))

(defun bard/search-notes-directory ()
  (interactive)
  (consult-grep "~/Notes/denote"))

(defvar bard/class-dirs
  '(("CSE 130 " . "~/Documents/Uni/SPRING2026-CSE 130/")
    ("CSE 310 " . "~/Documents/Uni/SPRING2026-CSE 310/")
    ("ENGR 111" . "~/Documents/Uni/SPRING2026-ENGR 111/")
    ("MATH 206" . "~/Documents/Uni/SPRING2026-MATH 206/")
    ("PHYS 298" . "~/Documents/Uni/SPRING2026-PHYS 298/"))
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
  "Return string for daily tasks heading in `denote-journal' entries."
  (with-temp-buffer
    (org-mode)
    (insert (format "* Tasks for %s\n** Время я потратил бездельничая\n\n* Notes for today\n\n* Clocktable\n"
                    (format-time-string "%Y-%m-%d (%a)")))
    (let ((org-clock-clocktable-default-properties
           '(:scope file :maxlevel 3 :link nil :compact t)))
      (org-clock-report))
    (buffer-string)))

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
