(require 'bard-calendar)
(require 'hi-lock)

(setq org-directory "~/Notes/denote/")
;; symlinked file to shorten denote file name in agenda buffers
(setq org-agenda-files (list "~/Notes/denote/todo.org" "~/Notes/denote/uni.org"))

;; Calendar
(use-package calendar-mode
  :config
  (setq calendar-holidays (append calendar-holidays russian-holidays))
  :hook
  (calendar-today-visible . calendar-mark-today)
  (calendar-mode . denote-journal-calendar-mode)
  (calendar-today-visible . calendar-mark-holidays))

(use-package orthodox-christian-new-calendar-holidays
  :ensure t
  :config
  (setq holiday-other-holidays (append holiday-other-holidays orthodox-christian-new-calendar-holidays))

  (setq holiday-bahai-holidays nil
        holiday-christian-holidays nil
        holiday-islamic-holidays nil))

;; Org todo keywords - changed to using hl-todo faces fixed by modus/ef themes
(setq org-todo-keywords
      '((sequence "TODO(t)" "EXTRA(e)" "INPROG(i)" "|" "DONE(d)" "KILLED(k)")
        (sequence "MEET(m)" "TENT(T)" "|" "MET(M)" "NOGO(n)")))

(setq org-todo-keyword-faces
      '(("EXTRA" . (:inherit warning))
        ("TENT" . (:inherit warning))
        ("MEET" . (:inherit warning underline bold))
        ("INPROG" . (:inherit hi-yellow :weight bold))))

(setq org-enforce-todo-dependencies t)

;; Automatically clock in
(add-hook 'org-after-todo-state-change-hook #'bard/auto-clock-in)

;; clock tables
(setq org-clock-clocktable-default-properties '(:maxlevel 7 :scope agenda)
      org-clock-persist 'history
      org-clock-mode-line-total 'current)
(org-clock-persistence-insinuate)

(use-package org
  ;; not really show what this does anymore
  :demand t
  :hook
  ((org-clock-out . bard/org-clock-update-mode-line)))

(global-set-key (kbd "<f1>") 'bard/default-agenda)
(global-set-key (kbd "M-<f1>") 'bard/choose-agenda)

(setq org-agenda-include-diary t)
(setq org-agenda-custom-commands
      `(("D" "Daily agenda and top priority tasks"
         ((tags-todo "!TODO"
                     ((org-agenda-overriding-header "Unscheduled Tasks \n")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day nil)
                      (org-deadline-warning-days 14)
                      ;; (org-scheduled-past-days 0)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "Today's agenda \n")))
          (agenda "" ((org-agenda-span 8)
                      (org-calendar-holiday)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this week \n")))))
        ("Y" "Yearly view for all tasks"
         ((agenda "" ((org-agenda-span 365)
                      (org-deadline-warning-days 2)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this Year\n")))))
        ("M" "Monthly view for all tasks"
         ((agenda "" ((org-agenda-span 31)
                      (org-deadline-warning-days 2)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this month\n")))))))

(use-package org-habit
  :after org-agenda
  :config
  (setq org-habit-show-done-always-green t
        org-habit-show-habits t
        org-habit-show-all-today t))

(provide 'bard-emacs-calendar)
