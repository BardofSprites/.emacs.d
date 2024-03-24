;; Modeline
(setq display-time-format "%Y-%m-%d (%A) %H:%M")
(setq display-time-interval 60)
(setq display-time-default-load-average nil)
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)
(setq display-time-string-forms
      '((propertize
         (format-time-string display-time-format now)
         'face 'display-time-date-and-time
         'help-echo (format-time-string "%a %b %e, %Y" now))
        " "))
(display-time-mode 1)

;; world clock
(setq world-clock-list
      '(("America/New_York" "New York")
	("Europe/Moscow" "Moscow")
	("Europe/London" "London")
	("Asia/Tokyo" "Tokyo")))

(setq world-clock-time-format "%Y-%m-%d %B (%A) %R %Z")

(define-key global-map (kbd "C-c C-w") #'world-clock)

;; timer package
(use-package tmr
  :ensure t
  :config
  (setq tmr-sound-file "/home/bard/.local/bin/scripts/bell.mp3")
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list 'tmr-description-history)
  (define-key global-map (kbd "C-c t l") 'tmr-tabulated-view)
  (define-key global-map (kbd "C-c t t") #'tmr)
  (define-key global-map (kbd "C-c t T") #'tmr-with-description)
  (define-key global-map (kbd "C-c t l") #'tmr-tabulated-view)
  (define-key global-map (kbd "C-c t c") #'tmr-clone)
  (define-key global-map (kbd "C-c t k") #'tmr-cancel)
  (define-key global-map (kbd "C-c t s") #'tmr-reschedule)
  (define-key global-map (kbd "C-c t e") #'tmr-edit-description)
  (define-key global-map (kbd "C-c t r") #'tmr-remove)
  (define-key global-map (kbd "C-c t R") #'tmr-remove-finished))

(provide 'bard-emacs-time)
