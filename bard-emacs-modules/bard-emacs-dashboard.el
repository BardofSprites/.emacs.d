;; (require 'bard-dashboard)

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (define-key global-map (kbd "<home>") #'dashboard-open)
;;   (define-key global-map (kbd "C-z d") #'dashboard-open)
;;   (setq initial-buffer-choice 'dashboard-open)
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner "~/.emacs.d/img/emacslogo.png")
;;   (setq dashboard-banner-logo-width 50)
;;   (setq dashboard-banner-logo-height 50)
;;   (setq dashboard-center-content t)
;;   ;; dashboar items
;;   (setq dashboard-items '((recents   . 5)
;; 			  (agenda . 5)))

;;   ;; Insert custom item
;;   (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
;;   (add-to-list 'dashboard-items '(custom) t)

;;   (setq dashboard-banner-logo-title "Time for another recreational programming session.")
;;   (setq dashboard-startupify-list '(dashboard-insert-banner
;; 				    dashboard-insert-newline
;; 				    dashboard-insert-banner-title
;; 				    dashboard-insert-newline
;; 				    dashboard-insert-items
;; 				    dashboard-insert-newline))

;;   (setq dashboard-set-init-info nil))

;; (provide 'bard-emacs-dashboard)
