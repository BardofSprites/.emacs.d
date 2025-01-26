fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(NOW (format-time-string "%Y-%0m-%0d %a %H:%0M"))
(hugo-time (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
(time (format-time-string "%Y-%0m-%0d %a %H:%0M"))
(yesterday (format-time-string "<%Y-%0m-%0d %a>" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "<%Y-%0m-%0d %a>" (time-add nil (* 24 60 60))))
(mode "-*- mode: " (s mode) " -*-")

c++-mode
(log "std::cout << \"" p "\" << std::endl;")

org-mode

(title "#+title: "p n "#+date: "(format-time-string "[%Y-%0m-%0d %a %H:%0M]") n)
(begin "\\begin{" (s env) "}" n> r> n> "\\end{" (s env) "}")
(eq "\\begin{equation}" n> r> n> "\\end{equation}")
(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(eg "(e.g. " p ")")
(fr "\\frac{" p "}{" p "}" q)
(NOW (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))
(time (format-time-string "\[%Y-%0m-%0d %a %H:%0M\]"))
(hat1 "$\\hat{\\texbg{"p q"}}$")
(hat2 "\\hat{\\texbg{"p q"}}")
(physics-lab "#+options: toc:nil num:1" n
             "#+setupfile: setup.org" n
             "#+LATEX_CLASS: article" n
             "#+export_file_name: Daniel Pinkston - " (car denote-title-history))

emacs-lisp-mode

(up "(use-package "p n> ":ensure t" n> (s configuration) q")")

(key "(define-key " p " (kbd \""p "\") #'"p")")

(hook "(add-hook '"(s hook-name) " #'" (s function-to-add) ")")

sh-mode
(! & "#!/usr/bin/bash" n q)

perl-mode
(! & "#!/usr/bin/perl" n "use strict;" n "use warnings;" n n q)

conf-space-mode
(func "DestroyFunc " p n "AddToFunc " p n q)
