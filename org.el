;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|

;; Org Variables
(setq org-directory "~/Notes/Org-Roam/")
(setq org-agenda-files (list "~/Notes/Org-Roam/todo.org"))

;; Making org mode look nice
(setq org-startup-indented t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300))

;; Org todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAIT" "DONE")))

;; Org Agenda
(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "All Tasks \n")))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day nil)
                      (org-deadline-warning-days 0)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "Today's agenda \n")))
          ;; write skip function that skips saturdays and sundays
          (agenda "" ((org-agenda-span 7)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this week \n")))))
        ("Y" "Monthly view for all tasks"
         ((agenda "" ((org-agenda-span 365)
                      (org-deadline-warning-days 2)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this Year\n")))))
        ("S" "Monthly view for all tasks"
         ((agenda "" ((org-agenda-span 31)
                      (org-deadline-warning-days 2)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this month\n")))))))

;; Org capture templates
(setq org-capture-templates
      '(("h" "Homework" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Homework")
         "* TODO %?")
	("e" "Extra/Coding" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Extra/Coding")
         "* TODO %?")
        ("r" "Reading List" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Watch/Read List")
         "* %?")
        ("j" "Journal" entry (file+datetree "~/Notes/Org-Roam/journal.org")
         "* %U %^{Title}\n  %?")
        ("a" "Appointment" entry (file+heading "~/Notes/Org-Roam/todo.org" "Appointment")
         "* %^{Task}\n  %^t\n  %?")))

;; Org Roam capture templates
(setq org-roam-capture-templates
      '(("d" "default" plain
         "\n* Tags: \n%? \n\n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ("n" "notes" plain
         "\n\n\n* Tags :: %? \n\n* ${title} \n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ("b" "bio" plain
         "#+ANKI_DECK: Bio \n\n* Tags :: [[id:cfe7bda9-b154-4d6b-989f-6af778a98cbd][Biology]] \n\n* %? \n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ("u" "apush" plain
         "#+ANKI_DECK: APUSH \n\n* Tags :: [[id:06334c1d-5c06-4b70-bfd8-a074c0c36706][APUSH]] \n\n* %? \n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ("s" "snapshot" plain
         (file "~/Notes/Org/snapshot_template.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ("i" "idea" plain
         "\n* Tags: \n%? \n\n"
         :if-new (file+head "Ideas/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)))
