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
        ("C-c l" . org-id-get-create)
        ("C-c j" . org-goto)
        )
  :bind
  (("C-c c" . org-capture))
  :config
  (setq org-goto-interface 'outline-path-completion)
  (setq org-special-ctrl-a/e t)
  (setq safe-local-variable-values '((org-refile-targets (nil :maxlevel . 3)))))

(setq org-archive-location "~/Notes/denote/20240328T215840--archive__self.org::* Archive")
(setq org-log-done 'time)
(setq org-icalendar-include-todo t
      org-icalendar-include-body t
      org-icalendar-with-timestamps t
      org-icalendar-use-scheduled '(event-if-todo-not-done)
      org-icalendar-scheduled-summary-prefix "SCHEDULED: "
      org-icalendar-use-deadline '(event-if-todo-not-done)
      org-icalendar-deadline-summary-prefix "DEADLINE: ")

(setq org-structure-template-alist
      '(("c" . "center")
        ("x" . "example")
        ("d" . "definition")
        ("t" . "theorem")
        ("q" . "quote")
        ("v" . "verse")
        ("s" . "src")
        ("E" . "src emacs-lisp :results value code :lexical t") ; for code examples in notes
        ("z" . "src emacs-lisp :tangle FILENAME") ; tangle without making dir, below makes dir
        ("Z" . "src emacs-lisp :tangle FILENAME :mkdirp yes")))
(setq org-ellipsis " ⤶")

(setq org-startup-indented t
      org-startup-folded 'showeverything
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(600)
      org-list-allow-alphabetical t
      org-insert-heading-respect-content t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "🞛" "◇" "◆")))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(defun bard/org-export-on-save ()
  "Export current Org buffer to PDF and open it with auto-revert enabled."
  (when (derived-mode-p 'org-mode)
    (org-latex-export-to-pdf)))

(define-minor-mode bard/org-auto-export-pdf-mode
  "Automatically export Org buffer to PDF on save."
  :lighter " AutoPDF"
  :group 'org
  (if bard/org-auto-export-pdf-mode
      (add-hook 'after-save-hook #'bard/org-export-on-save)
    (remove-hook 'after-save-hook #'bard/org-export-on-save)))

(use-package auctex
  :ensure t)

(use-package cdlatex
  :ensure t
  )

;; latex editing niceness
(use-package org-fragtog
  :ensure t)

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

(setq org-latex-listings t)
(setq org-latex-listings-options
      '(("basicstyle" "\\ttfamily")
        ("breakatwhitespace" "false")
        ("breakautoindent" "true")
        ("breaklines" "true")
        ("columns" "[c]fullflexible")
        ("commentstyle" "")
        ("emptylines" "*")
        ("extendedchars" "false")
        ("fancyvrb" "true")
        ("firstnumber" "auto")
        ("flexiblecolumns" "false")
        ("frame" "single")
        ("frameround" "tttt")
        ("identifierstyle" "")
        ("keepspaces" "true")
        ("keywordstyle" "")
        ("mathescape" "false")
        ("numbers" "left")
        ("numbers" "none")
        ("numbersep" "5pt")
        ("numberstyle" "\\tiny")
        ("resetmargins" "false")
        ("showlines" "true")
        ("showspaces" "false")
        ("showstringspaces" "false")
        ("showtabs" "true")
        ("stepnumber" "2")
        ("stringstyle" "")
        ("tab" "↹")
        ("tabsize" "4")
        ("texcl" "false")
        ("upquote" "false")))

(setq org-capture-bookmark nil
      org-id-link-to-org-use-id t)

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
          "~/Notes/denote/uni.org")
         (file
          "~/Notes/denote/templates/class-template.org"))
        ("p" "project idea" entry
         (file
          "~/Notes/denote/20250201T165619--project-ideas__idea_programming.org")
         "* %^{Project description}\n%?")))

;; copy/paste images
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

;; pdf notes
(use-package org-noter
  :ensure t)

;; links
(use-package org-cliplink
  :ensure t
  :bind
  ("C-c p" . org-cliplink))

(provide 'bard-emacs-org)
