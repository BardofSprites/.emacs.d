 (defun dashboard-insert-custom (list-size)
	"Insert custom itemes LIST-SIZE."
	(interactive)
	(insert "  TODOs (A)   Cal: (c) ♪ Music: (m)   Mail: (M)   Emacs: (e)"))

(add-hook 'dashboard-mode-hook
          (lambda()
            (define-key dashboard-mode-map (kbd "A") #'(lambda ()(interactive)(org-agenda nil "D")))
	    ;; FIXME look at browse url for explanation
	    ;; (define-key global-map (kbd "g") #')
	    (define-key dashboard-mode-map (kbd "c") #'calendar)
	    (define-key dashboard-mode-map (kbd "M") #'notmuch)
	    (define-key dashboard-mode-map (kbd "e") #'(lambda ()(interactive)(dired user-emacs-directory)))
	    (define-key dashboard-mode-map (kbd "m") #'(lambda ()(interactive)(emms)))
	    ))

(defvar dashboard-recover-layout-p nil
  "Whether recovers the layout.")

(defun open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)
  (setq dashboard-recover-layout-p t)
  (delete-other-windows)
  (dashboard-refresh-buffer)
  (dashboard-goto-recent-files))

(defun quit-dashboard ()
  "Quit dashboard window."
  (interactive)
  (quit-window t)
  (when (and dashboard-recover-layout-p
	     (bound-and-true-p winner-mode))
    (winner-undo)
    (setq dashboard-recover-layout-p nil)))

;; TODO replace with browse-url librewolf that opens to localhost new tab
;; (defun bard/open-librewolf ()
;;   (interactive)
;;   (if (string-match "\\`bardiel" system-name)
;;     (async-shell-command "librewolf-bin"))
;;   '(async-shell-command "librewolf"))

;; TODO replace this with browse-url that opens to localhost new tab
;; (defun bard/open-firefox ()
;;   (interactive)
;;   (if (string-match "\\`bardiel" system-name)
;;     (async-shell-command "firefox-bin"))
;;   '(async-shell-command "firefox"))

;; FIXME browse-url-librewolf is incomplete
;; (defun bard/open-user-github ()
;;   (browse-url-librewolf 'user-github-url))

(provide 'bard-dashboard)
