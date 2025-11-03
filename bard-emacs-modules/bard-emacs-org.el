;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|
(require 'org)
(require 'ox)
(require 'org-habit)

(use-package org
  :defer nil
  :bind
  (:map org-mode-map
        ("C-M-a" . backward-paragraph)
        ("C-M-e" . forward-paragraph)
        ("C-c M-c" . count-words-region)
        ("C-c C-M-c" . count-words)
        ("C-c l" . org-store-link)
        ("C-c j" . org-goto)
        )
  :bind
  (("C-c c" . org-capture))
  :config
  (setq org-goto-interface 'outline-path-completion)
  (setq safe-local-variable-values '((org-refile-targets (nil :maxlevel . 3))))
  ;; :hook
  ;; ((org-mode . org-num-mode))
  )

;; Org Variables
(setq bard/org-anki-file "~/Notes/denote/20240729T171836--anki-flashcards__cards_meta.org")
(setq org-archive-location "~/Notes/denote/20240328T215840--archive__self.org::* Archive")
(setq org-log-done 'time)
(setq org-icalendar-include-todo t
      org-icalendar-include-body t
      org-icalendar-with-timestamps t
      org-icalendar-use-scheduled '(event-if-todo-not-done)
      org-icalendar-scheduled-summary-prefix "SCHEDULED: "
      org-icalendar-use-deadline '(event-if-todo-not-done)
      org-icalendar-deadline-summary-prefix "DEADLINE: ")

(setq org-habit-show-all-today nil)

(setq org-structure-template-alist
      '(("c" . "center")
	    ("x" . "example")
	    ("q" . "quote")
	    ("v" . "verse")
	    ("s" . "src")
        ("E" . "src emacs-lisp :results value code :lexical t") ; for code examples in notes
        ("t" . "src emacs-lisp :tangle FILENAME") ; tangle without making dir, below makes dir
        ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")))
(setq org-ellipsis " ⤶")

;; mainly for denote, org throws away a link that i might reuse later
(setq org-id-link-to-org-use-id t)
(setq org-link-keep-stored-after-insertion nil)

;; Making org mode look nice
(setq org-startup-indented t
      org-startup-folded 'showeverything
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(600)
      org-list-allow-alphabetical t
      org-insert-heading-respect-content t
      org-special-ctrl-a/e t)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(use-package auctex
  :ensure t)

(use-package cdlatex
  :ensure t
  )

;; (use-package org-mode
;;   :config

;;   ;; (setq org-latex-to-pdf-process
;;   ;;       '("xelatex -interaction nonstopmode %f"
;;   ;;         "xelatex -interaction nonstopmode %f"))
;;   ;; (add-to-list 'org-latex-packages-alist
;;   ;;              '("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex")))
;;   ;; (add-to-list 'org-latex-packages-alist
;;   ;;              '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
;; )

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; latex editing niceness
(use-package org-fragtog
  :ensure t)

;;; Org capture
(setq org-capture-bookmark nil)

(require 'org-protocol)
(setq org-capture-templates
      '(("t" "task" entry
         (file+olp
          "~/Notes/denote/20240328T215727--todo.org"
          "Inbox" "General tasks")
	     "* TODO %?")
        ;; ("s" "Basic Statistics" entry
        ;;  (file+headline
        ;;   "~/Notes/denote/20240830T215644--statistics-flashcards__anki_stats.org" "Unsorted")
        ;;  "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Statistics\n:END:\n*** Front\n %?\n*** Back\n\n")
        ;; ("S" "Cloze Statistics" entry
        ;;  (file+headline
        ;;   "~/Notes/denote/20240830T215644--statistics-flashcards__anki_stats.org" "Unsorted")
        ;;  "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Statistics\n:END:\n*** Text\n %?\n*** Hooray\n\n")
        ("c" "Basic Chemistry" entry
         (file+headline
          "~/Notes/denote/20251019T175402--chemistry-flashcards__anki_chem.org" "Unsorted")
         "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Chemistry\n:END:\n*** Front\n%?\n*** Back\n\n")
        ("n" "common place note" entry
         (file "~/Notes/denote/20251023T182240--common-place-notes__topic.org")
         "* %^{Source}\n#+BEGIN_QUOTE\n%?\n#+END_QUOTE")
        ("z" "Protocol" entry
         (file+olp
          "~/Notes/denote/20240328T220037--media-tracker__media_topic.org" "Quotes")
         "* Source: [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
	    ("Z" "Protocol Link" entry
         (file+olp
          "~/Notes/denote/20240328T220037--media-tracker__media_topic.org" "Watch/Read List")
	     "* [[%:link][%:description]] \nCaptured On: %U \n%?")
        ("w" "Class outline" entry
         (file
          "~/Notes/denote/20240328T215727--todo.org")
         (file
          "~/Notes/denote/templates/class-template.org"))
        ("p" "project idea" entry
         (file
          "~/Notes/denote/20250201T165619--project-ideas__idea_programming.org")
         "* %^{Project description}\n%?")))

;;; Org Publish
(setq org-html-scripts nil)
(setq org-publish-project-alist
      '(("org-blog"
         :base-directory "~/Code/org-blog/"
         :base-extension "org"
         :publishing-directory "~/Code/org-site/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-preamble "<p class=\"backlink\"><a href=\"index.html\">Go back to note index</a></p><p class=\"updatedate\">Page last updated: <i>%d</i></p><hr>"
         :html-postamble nil)))

;;; Managing media
;; inspired by https://zzamboni.org/post/how-to-insert-screenshots-in-org-documents-on-macos/

(use-package org-download
  :after org
  :defer nil
  :ensure t
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/Notes/denote/Images")
  (org-download-heading-lvl 0)
  (org-download-timestamp "org_%Y%m%d-%H%M%S_")
  (org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'")
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "🞛" "◇" "◆"))
  (set-fontset-font t 'symbol (font-spec :family "Iosevka Comfy") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Iosevka Comfy") nil 'append))

(provide 'bard-emacs-org)
