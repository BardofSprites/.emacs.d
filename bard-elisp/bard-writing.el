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

(use-package consult-denote
  :ensure t
  :bind
  ("C-c n g" . consult-denote-grep)
  ("C-c n f" . consult-denote-find)
  :config
  (setq consult-denote-find-command 'consult-fd
        consult-denote-grep-command 'consult-ripgrep))

(defvar bard/class-dirs
  '(("COMM 111" . "~/Documents/Uni/SUMMER2026-COMM 111/")
    ("CSE 220" . "~/Documents/Uni/SUMMER2026-CSE 220/")
    ("PHYS 296" . "~/Documents/Uni/SUMMER2026-PHYS 296/")
    ("PHYS 299" . "~/Documents/Uni/SUMMER2026-PHYS 299/"))
  "Mapping of class names to their document directories.")

(defvar bard/uni-notes-file "~/Notes/denote/uni.org"
  "Path to the main university org file.")

(defun bard/jump-to-class (class &optional with-dired)
  "Jump to CLASS heading in bard/uni-notes-file, can open its dir in Dired."
  (interactive
   (list
    (completing-read "Class: " (mapcar #'car bard/class-dirs))
    current-prefix-arg))
  (let ((dir (cdr (assoc class bard/class-dirs))))
    (delete-other-windows)
    (find-file bard/uni-notes-file)
    (widen)
    (goto-char (point-min))
    (search-forward class nil nil)

    (when with-dired
      (let ((dired-window (split-window-right)))
        (with-selected-window dired-window
          (dired dir))))))

(defun bard/jump-to-class-new-frame (class &optional with-dired)
  "Open CLASS notes and dir in a new frame titled after CLASS, optionally WITH-DIRED."
  (interactive
   (list
    (completing-read "Class: " (mapcar #'car bard/class-dirs))
    current-prefix-arg))
  (let* ((dir (cdr (assoc class bard/class-dirs)))
         (frame (make-frame `((frame-title-format . ,class)))))
    (select-frame-set-input-focus frame)
    (delete-other-windows)
    (find-file bard/uni-notes-file)
    (widen)
    (goto-char (point-min))
    (search-forward class nil nil)

    (when with-dired
      (let ((dired-window (split-window-right)))
        (with-selected-window dired-window
          (dired dir)
          (beframe-rename-current-frame))))))

;; Optional: bind to a key
(global-set-key (kbd "C-c u") #'bard/jump-to-class)
(global-set-key (kbd "C-c U") #'bard/jump-to-class-new-frame)

(defvar bard/subheading-keys
  '(("schedule"  . "s")
    ("check in"  . "c")
    ("homework"  . "h")
    ("notes"     . "n")
    ("exams"     . "e")
    ("todos"     . "t"))
  "Fixed key bindings for known class subheadings in the transient menu.")

(defun bard/get-class-subheadings (class)
  "Return a list of level-2 subheading titles under CLASS in `bard/uni-notes-file'."
  (with-current-buffer (find-file-noselect bard/uni-notes-file)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (search-forward class nil nil)
      (let ((class-end (save-excursion
                         (org-end-of-subtree t)
                         (point)))
            (subheadings '()))
        (while (re-search-forward "^\\*\\* \\(.+\\)" class-end t)
          (push (match-string-no-properties 1) subheadings))
        (nreverse subheadings)))))

(defun bard/jump-to-class-subheading (class subheading &optional with-dired)
  "Jump to SUBHEADING under CLASS in `bard/uni-notes-file'."
  (let ((dir (cdr (assoc class bard/class-dirs))))
    (delete-other-windows)
    (find-file bard/uni-notes-file)
    (widen)
    (goto-char (point-min))
    (search-forward class nil nil)
    (let ((class-pos (point)))
      (if (re-search-forward
           (concat "^\\*\\* " (regexp-quote subheading)) nil t)
          (progn
            (org-fold-show-entry)
            (org-narrow-to-subtree))
        (goto-char class-pos)))
    (when with-dired
      (let ((dired-window (split-window-right)))
        (with-selected-window dired-window
          (dired dir))))))

(defun bard/jump-to-class-section ()
  "Pick a class then a subheading, both via completing-read."
  (interactive)
  (let* ((class    (completing-read "Class: " (mapcar #'car bard/class-dirs) nil t))
         (sections (bard/get-class-subheadings class))
         (section  (completing-read "Section: " sections nil t))
         (dired-p  current-prefix-arg))
    (bard/jump-to-class-subheading class section dired-p)))

(defvar bard/--current-class nil
  "Class selected in the first step of bard/class-menu.")


(transient-define-prefix bard/--section-menu ()
  "Pick a subheading within `bard/--current-class'."
  [:description
   (lambda () (format "Sections in %s" bard/--current-class))
   :class transient-column
   :setup-children
   (lambda (_)
     (let ((sections (bard/get-class-subheadings bard/--current-class)))
       (delq nil
             (mapcar
              (lambda (section)
                (let ((key (cdr (assoc section bard/subheading-keys))))
                  (when key
                    (transient-parse-suffix
                     'bard/--section-menu
                     `(,key ,section (lambda () (interactive)
                                       (bard/jump-to-class-subheading
                                        bard/--current-class ,section)))))))
              sections))))])

;; TODO make some kind of dired menu after selecting the class
;; (transient-define-prefix bard/--dired-menu ()
;;   "Pick an action relating to dired."
;;   [:description
;;    (lambda () (format "Open dired in" bard/--current-class))
;;    :class transient-column
;;    :setup-children
;;    (lambda
;;      )])

(transient-define-prefix bard/class-menu ()
  "Jump to a class section via transient menus."
  ["Classes"
   :class transient-column
   :setup-children
   (lambda (_)
     (let ((i 1))
       (mapcar
        (lambda (entry)
          (let* ((class (car entry))
                 (key   (number-to-string i)))
            (setq i (1+ i))
            (transient-parse-suffix
             'bard/class-menu
             `(,key ,class (lambda () (interactive)
                             (setq bard/--current-class ,class)
                             (bard/--section-menu))))))
        bard/class-dirs)))])

(defun denote-typst-with-signature ()
  "Create a Typst note while prompting for template and signature."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-file-type 'typst)
        (denote-prompts
         (denote-add-prompts '(signature template))))
    (call-interactively #'denote)))

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
   '((?V "\\vec")
     (?s "\\sigma" "\\text{ s.t. }" "\\sin")
     (?= "\\implies" "\\Leftrightarrow" "\\Longleftrightarrow"))))

;; (use-package xenops
;;   :ensure t
;;   :config
;;   (setq xenops-math-image-scale-factor 1.2))

(define-minor-mode bard/org-math-mode
  "Enable features to write math in `org-mode'."
  :init-value nil
  :lighter " S="
  :global nil
  (xenops-mode t)
  (org-cdlatex-mode t)
  (electric-pair-local-mode t)
  (bard/cdlatex-add-math-symbols)
  ;; sending math to calc from latex doc
  (define-key org-mode-map (kbd "C-S-e") #'latex-math-from-calc))

;; latex into calc
(defun latex-math-from-calc ()
  "Evaluate `calc' on the contents of line at point."
  (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval `(,string calc-language latex
                                        calc-prefer-frac t
                                        calc-angle-mode rad)))))
        (t (let ((l (thing-at-point 'line)))
             (end-of-line 1) (kill-line 0)
             (insert (calc-eval `(,l
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))))

(provide 'bard-writing)
