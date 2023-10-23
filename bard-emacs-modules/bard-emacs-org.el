;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|

(require 'bard-emacs-ui)

;; Org Variables
(setq org-directory "~/Notes/Org-Roam/")
(setq org-agenda-files (list "~/Notes/Org-Roam/todo.org"))
(setq org-archive-location "~/Notes/Org-Roam/archive.org::* Archive")
(setq org-log-done 'time)

;; Making org mode look nice
(setq org-startup-indented t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-image-actual-width '(300))

;; Org todo keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILLED(k)"))
      org-todo-keyword-faces '(("TODO"   . (:weight bold :foreground "#CC9393" :weight bold))
			       ("WAIT"   . (:weight bold :foreground "#DFAF8F" :weight bold))
			       ("DONE"   . (:weight bold :weight bold :foreground "#AFD8AF"))
			       ("KILLED" . (:weight bold :weight bold :foreground "#656555"))))

;; Org Cliplink
(global-set-key (kbd "C-x p i") 'org-cliplink)

;; Org Agenda Faces
(custom-set-faces '(org-agenda-structure ((t (:inherit bold :foreground "#70a89f" :height 1.5 :family "Iosevka Comfy Motion Duo")))))

;; Org Agenda
(defun bard/choose-agenda ()
  "For viewing my custom agenda"
  (interactive)
  (let ((agenda-views '("Default" "Monthly" "Yearly")))
    (setq chosen-view (completing-read "Choose an agenda view: " agenda-views))
  (cond
      ((string= chosen-view "Yearly")
       (org-agenda nil "Y"))
      ((string= chosen-view "Monthly")
       (org-agenda nil "M"))
      ((string= chosen-view "Default")
       (org-agenda nil "D")))))

(global-set-key (kbd "<f6>") 'bard/choose-agenda)

;; Org Agenda
(setq org-agenda-custom-commands
      `(("D" "Daily agenda and top priority tasks"
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

;; Org capture templates
(define-key global-map (kbd "C-`") #'org-capture)

(require 'org-protocol)
(setq org-capture-templates
      '(("h" "Homework" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Homework")
         "* TODO %?")
	("e" "Extra/Coding" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Extra/Coding")
         "* TODO %?")
        ("r" "Reading List" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Immediate Reading List")
         "* %?")
	("p" "Protocol" entry (file+olp "~/Notes/Org-Roam/media.org" "Quotes")
         "* Source: [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
	("L" "Protocol Link" entry (file+olp "~/Notes/Org-Roam/media.org" "Watch/Read List")
         "* [[%:link][%:description]] \nCaptured On: %U \n%?")
        ("j" "Journal" entry (file+datetree "~/Notes/Org-Roam/journal.org")
         "* %U %^{Title}\n  %?")
        ("a" "Appointments" entry (file+olp "~/Notes/Org-Roam/todo.org" "Appointments")
         "* %^{Appointment}\n  %^t\n  %?")
	("c" "Archive" entry (file+olp "~/Notes/Org-Roam/archive.org" "Archive")
	 "* %^{Task}\nCaptured On: %U")))

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
