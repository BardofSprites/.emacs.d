;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|
(require 'org)
(require 'ox)

;; Org Variables
(setq org-directory "~/Notes/denote/")
;; symlinked file to shorten denote file name in agenda buffers
(setq org-agenda-files (list "~/Notes/denote/todo.org"))
(setq org-archive-location "~/Notes/denote/20240328T215840--archive__self.org::* Archive")
(setq org-log-done 'time)
(setq org-structure-template-alist
      '(("c" . "center")
	("x" . "example")
	("q" . "quote")
	("v" . "verse")
	("s" . "src")
        ("E" . "src emacs-lisp :results value code :lexical t") ; for code examples in notes
        ("t" . "src emacs-lisp :tangle FILENAME") ; tangle without making dir, below makes dir
        ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")))

;; mainly for denote, org throws away a link that i might reuse later
(setq org-link-keep-stored-after-insertion nil)

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
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "KILLED(k)")
	(sequence "MEET(m)" "|" "MET(M)")))

;; Org Mode Key map
(defun bard/org-mode-keybindings ()
  (define-key org-mode-map (kbd "C-c a") 'org-table-align))

(add-hook 'org-mode-hook 'bard/org-mode-keybindings)
;; (add-hook 'org-mode-hook 'fly-spell-mode)

;; Org Agenda Faces
(custom-set-faces '(org-agenda-structure ((t (:inherit bold :height 1.5 :family "Iosevka Comfy Motion")))))
(setq org-ellipsis "↲")

;;; Org Agenda

;; clock tables
(setq org-clock-clocktable-default-properties '(:maxlevel 7
						:scope agenda))
(defun bard/org-clock-report ()
  (interactive)
  (bard/new-org-buffer)
  (org-clock-report))

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
	 ((tags-todo "!TODO"
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
	  (tags "+wait"
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

(use-package org-alert
  :init
  (org-alert-enable)
  :config
  (setq alert-default-style 'libnotify))

(use-package orthodox-christian-new-calendar-holidays
  :ensure t
  :config
  (setq holiday-other-holidays (append holiday-other-holidays orthodox-christian-new-calendar-holidays))

  (setq holiday-bahai-holidays nil
	holiday-christian-holidays nil
	holiday-islamic-holidays nil))

;;; Org capture
(setq org-capture-bookmark nil)

(define-key global-map (kbd "C-c c") #'org-capture)

(require 'org-protocol)
(setq org-capture-templates
      '(("i" "Important Stuff" entry (file+olp "~/Notes/denote/20240328T215727--todo__self.org" "Inbox" "Important Stuff")
	 "* TODO %?")
	("e" "Extra/Coding" entry (file+olp "~/Notes/denote/20240328T215727--todo__self.org" "Inbox" "Extra/Coding")
	 "* TODO %?")
	("j" "Journal" entry (file+datetree "~/Notes/denote/20240328T215351--journal__journal_self.org")
	 "* %U %^{Title}\n  %?")
	("a" "Appointments" entry (file+olp "~/Notes/denote/20240328T215727--todo__self.org" "Appointments" "General")
	 "* MEET %^{Appointment}\nSCHEDULED: %^t\n%?")
	("p" "Protocol" entry (file+olp "~/Notes/denote/20240328T220037--media-tracker__self.org" "Quotes")
	 "* Source: [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
	("L" "Protocol Link" entry (file+olp "~/Notes/denote/20240328T220037--media-tracker__self.org" "Watch/Read List")
	 "* [[%:link][%:description]] \nCaptured On: %U \n%?")
	("b" "Blog Article" entry (file+olp "~/Code/bardmandev/content/_index.org" "Latest updates"))))

;; (provide 'bard-emacs-org)
