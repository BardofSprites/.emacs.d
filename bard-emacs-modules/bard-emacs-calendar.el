(require 'bard-calendar)

(setq org-directory "~/Notes/denote/")
;; symlinked file to shorten denote file name in agenda buffers
(setq org-agenda-files (list "~/Notes/denote/todo.org"))

;; Calendar
(use-package calendar-mode
  :config
  (setq calendar-holidays (append calendar-holidays russian-holidays))
  :hook
  (calendar-today-visible . calendar-mark-today)
  (calendar-today-visible . calendar-mark-holidays))

;; Org todo keywords - changed to using hl-todo faces fixed by modus/ef themes
(setq org-todo-keywords
      '((sequence "TODO(t)" "EXTRA(e)" "|" "DONE(d)" "KILLED(k)")
        (sequence "MEET(m)" "|" "MET(M)")))

(setq org-todo-keyword-faces
      '(("EXTRA" . (:inherit warning))))

;;; Org Agenda

;; clock tables
(setq org-clock-clocktable-default-properties '(:maxlevel 7 :scope agenda)
      org-clock-persist 'history
      org-clock-mode-line-total 'current)
(org-clock-persistence-insinuate)

(use-package org
  :demand t
  :hook
  ((org-clock-out . bard/org-clock-update-mode-line)))

(global-set-key (kbd "M-<f1>") 'bard/choose-agenda)
(global-set-key (kbd "C-c a c") 'bard/choose-agenda)

(global-set-key (kbd "<f1>") 'bard/default-agenda)
(global-set-key (kbd "C-c a a") 'bard/default-agenda)

;; Org Agenda
(setq org-agenda-include-diary t)
(setq org-agenda-custom-commands
      `(("D" "Daily agenda and top priority tasks"
         ((tags-todo "!TODO"
                     ((org-agenda-overriding-header "Unscheduled Tasks \n")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
          (tags-todo "-SCHEDULED<>\"\"+DEADLINE<>\"\""
                     ((org-agenda-sorting-strategy '(deadline-up priority-down))
                      (org-agenda-overriding-header "Deadline Items")
                      (org-agenda-prefix-format " %i %-12:c%?-12t% s")
                      (org-agenda-format-date "%A %-e %B %Y")))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day nil)
                      (org-deadline-warning-days 0)
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

(use-package orthodox-christian-new-calendar-holidays
  :ensure t
  :config
  (setq holiday-other-holidays (append holiday-other-holidays orthodox-christian-new-calendar-holidays))

  (setq holiday-bahai-holidays nil
        holiday-christian-holidays nil
        holiday-islamic-holidays nil))

(defun bard/export-agenda-to-file ()
  (interactive)
  (org-eval-in-environment (org-make-parameter-alist
                            '(org-agenda-span
                              'week
                              org-agenda-use-time-grid t
                              org-agenda-remove-tags t
                              org-agenda-window-setup 'nope))
    (let* ((wins (current-window-configuration))
           org-agenda-sticky)
      (save-excursion
        (with-current-buffer
            (get-buffer-create org-agenda-buffer-name)
          (pop-to-buffer (current-buffer))
          (org-agenda nil "a")
          (let ((result (buffer-string)))
            (with-temp-file "~/.cache/agenda" (insert result)))))
      (set-window-configuration wins))))

(provide 'bard-emacs-calendar)
