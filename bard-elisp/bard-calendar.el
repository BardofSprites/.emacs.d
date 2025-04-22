(require 'org)

;; Org Clock

(defun bard/auto-clock-in ()
  "Automatically clock in when task marked in progress (INPROG),
 and start study session."
  (when (equal (org-get-todo-state) "INPROG")
    (org-clock-in)
    (bard/study-session)))

(defun bard/study-session ()
  "Prompt for study parameters, run study session, and clock out when done."
  (interactive)
  (let* ((study-time (read-string "Study time (minutes): "))
         (break-time (read-string "Break time (minutes): "))
         (sessions (read-string "Number of sessions: "))
         (command (format "study %s %s %s" study-time break-time sessions))
         (process (start-process-shell-command "study-session" "*study*" command)))
    (set-process-sentinel
     process
     (lambda (_proc event)
       (when (string= event "finished\n")
         (progn (org-clock-out)
                (pop-to-buffer-same-window "todo.org")))))))

(defun bard/org-clock-report ()
  (interactive)
  (bard/new-org-buffer)
  (org-clock-report))

(defun bard/org-clock-update-mode-line ()
  (interactive)
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun bard/org-clock-task-string ()
  "Return a simplified org clock task string."
  (if (and (boundp 'org-mode-line-string)
           (not (string-equal "" org-mode-line-string))
           org-mode-line-string)
      (substring-no-properties org-mode-line-string)
    "No task clocked in"))

(defun bard/open-calendar ()
  "Opens calendar as only window"
  (interactive)
  (calendar)
  (delete-other-windows))

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

(defun bard/default-agenda ()
  "For viewing my custom agenda"
  (interactive)
  (org-agenda nil "D"))

(provide 'bard-calendar)
;;; bard-calendar.el ends here
