fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(NOW (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))
(yesterday (format-time-string "<%Y-%0m-%0d %a>" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "<%Y-%0m-%0d %a>" (time-add nil (* 24 60 60))))
(mode "-*- mode: " (s mode) " -*-")

emacs-lisp-mode

(up "(use-package "p n> ":ensure t" n> (s configuration)")")

(key "(define-key " p " (kbd \""p "\") #'"p")")

(hook "(add-hook '"(s hook-name) " #'" (s function-to-add) ")")
