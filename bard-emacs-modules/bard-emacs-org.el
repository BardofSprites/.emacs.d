;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|

(require 'bard-emacs-ui)
(require 'org)
(require 'ox)

;; Org Variables
(setq org-directory "~/Notes/Org-Roam/")
(setq org-agenda-files (list "~/Notes/Org-Roam/todo.org"))
(setq org-archive-location "~/Notes/Org-Roam/archive.org::* Archive")
(setq org-log-done 'time)

;; Making org mode look nice
(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(600)
      org-list-allow-alphabetical t
      org-insert-heading-respect-content t
      org-special-ctrl-a/e t)

(with-eval-after-load "org-mode"
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
  )

;; Org Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path "/home/bard/opt/plantuml/plantuml-1.2024.3.jar")

;; latex editing niceness
(use-package org-fragtog
  :ensure t)

;; Calendar

(with-eval-after-load 'calendar-mode
  (setq calendar-holidays (append calendar-holidays russian-holidays))
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-holidays))

(define-key global-map (kbd "C-z C-c") #'calendar)

;; Org todo keywords - changed to using hl-todo faces fixed by modus/ef themes
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "NEXT (n)" "|" "DONE(d)" "KILLED(k)")))
     (setq org-todo-keywords
           '((sequence "TODO(t)" "|" "DONE(D)" "KILLED(k)")
             (sequence "MEET(m)" "|" "MET(M)")))

;; Org Mode Key map
(defun bard/org-mode-keybindings ()
  (define-key org-mode-map (kbd "C-c a") 'org-table-align))

(add-hook 'org-mode-hook 'bard/org-mode-keybindings)
;; (add-hook 'org-mode-hook 'fly-spell-mode)

;; Org Agenda Faces
(custom-set-faces '(org-agenda-structure ((t (:inherit bold :height 1.5 :family "Iosevka Comfy Motion")))))
(setq org-ellipsis "↲")

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

(global-set-key (kbd "M-<f1>") 'bard/choose-agenda)
(global-set-key (kbd "C-z C-a") 'bard/choose-agenda)

(defun bard/default-agenda ()
  "For viewing my custom agenda"
  (interactive)
  (org-agenda nil "D"))

(global-set-key (kbd "<f1>") 'bard/default-agenda)
(global-set-key (kbd "C-z a") 'bard/default-agenda)

;; Org Agenda
(setq org-agenda-custom-commands
      `(("D" "Daily agenda and top priority tasks"
         ((tags-todo "!TODO/-WAIT"
                     ((org-agenda-overriding-header "Unscheduled Tasks \n")
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day nil)
                      (org-deadline-warning-days 0)
                      (org-scheduled-past-days 0)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "Today's agenda \n")))
          (agenda "" ((org-agenda-span 7)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this week \n")))
	  (tags-todo "-TODO/!WAIT"
                     ((org-agenda-overriding-header "Low Priority Tasks\n")
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))))
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

(use-package org-timeblock
  :ensure t
  :config
  (define-key global-map (kbd "C-z c") 'org-timeblock))

(use-package orthodox-christian-new-calendar-holidays
  :ensure t
  :config
  (setq holiday-other-holidays (append holiday-other-holidays orthodox-christian-new-calendar-holidays))

  (setq holiday-bahai-holidays nil
	holiday-christian-holidays nil
	holiday-islamic-holidays nil))

;; Org capture templates
(define-key global-map (kbd "C-c c") #'org-capture)

(require 'org-protocol)
(setq org-capture-templates
      '(("h" "Homework" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Important Stuff")
         "* TODO %?")
	("e" "Extra/Coding" entry (file+olp "~/Notes/Org-Roam/todo.org" "Inbox" "Extra/Coding")
         "* TODO %?")
	("p" "Protocol" entry (file+olp "~/Notes/Org-Roam/media.org" "Quotes")
         "* Source: [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
	("L" "Protocol Link" entry (file+olp "~/Notes/Org-Roam/media.org" "Watch/Read List")
         "* [[%:link][%:description]] \nCaptured On: %U \n%?")
        ("j" "Journal" entry (file+datetree "~/Notes/Org-Roam/journal.org")
         "* %U %^{Title}\n  %?")
        ("a" "Appointments" entry (file+olp "~/Notes/Org-Roam/todo.org" "Appointments")
         "* %^{Appointment}\n  %^t\n  %?")
	("b" "Blog Article" entry (file+olp "~/Code/bardmandev/content/_index.org" "Latest updates"))))

;;;; Org Roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Notes/Org-Roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :ensure t)

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
         "#+ANKI_DECK: Bio \n\n* Tags :: [[id:cfe7bda9-b154-4d6b-989f-6af778a98cbd][Biology]] \n\n* ${title}%? \n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ("u" "apush" plain
         "#+ANKI_DECK: APUSH \n\n* Tags :: [[id:06334c1d-5c06-4b70-bfd8-a074c0c36706][APUSH]] \n\n* ${title}%? \n"
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

(provide 'bard-emacs-org.el)
