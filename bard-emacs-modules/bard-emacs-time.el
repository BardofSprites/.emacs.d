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

(provide 'bard-emacs-time)
