;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|
(require 'org)
(require 'ox)
(require 'org-habit)

(use-package org-mode
  :bind
  (:map org-mode-map
        ("C-M-a" . backward-paragraph)
        ("C-M-e" . forward-paragraph)
        ("C-c M-c" . count-words-region)
        ("C-c C-M-c" . count-words)
        ("C-c l" . org-store-link)
        )
  :bind
  (("C-c c" . org-capture)))

;; Org Variables
(setq bard/org-anki-file "~/Notes/denote/20240729T171836--anki-flashcards__cards_meta.org")
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
(setq org-id-link-to-org-use-id t)
(setq org-link-keep-stored-after-insertion nil)

;; Making org mode look nice
(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(600)
      org-list-allow-alphabetical t
      org-insert-heading-respect-content t
      org-special-ctrl-a/e t)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

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
      '(("i" "Important Stuff" entry
         (file+olp
          "~/Notes/denote/20240328T215727--todo__self.org"
          "Inbox" "Important Stuff")
	     "* TODO %?")
	    ("e" "Extra/Coding" entry
         (file+olp
          "~/Notes/denote/20240328T215727--todo__self.org" "Inbox" "Extra/Coding")
	     "* TODO %?")
        ("s" "Basic Statistics" entry
         (file+headline
          "~/Notes/denote/20240830T215644--statistics-flashcards__anki_stats.org" "Unsorted")
         "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Statistics\n:END:\n*** Front\n %?\n*** Back\n\n")
        ("S" "Cloze Statistics" entry
         (file+headline
          "~/Notes/denote/20240830T215644--statistics-flashcards__anki_stats.org" "Unsorted")
         "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Statistics\n:END:\n*** Text\n %?\n*** Hooray\n\n")
        ("p" "Basic Physics" entry
         (file+headline
          "~/Notes/denote/20240902T144403--physics-flashcards__anki_physics.org" "Unsorted")
         "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Physics\n:END:\n*** Front\n %?\n*** Back\n\n")
        ("P" "Cloze Physics" entry
         (file+headline
          "~/Notes/denote/20240902T144403--physics-flashcards__anki_physics.org" "Unsorted")
         "** %U %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Physics\n:END:\n*** Text\n %?\n*** Hooray\n\n")
        ("z" "Protocol" entry
         (file+olp
          "~/Notes/denote/20240328T220037--media-tracker__self.org" "Quotes")
         "* Source: [[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
	    ("Z" "Protocol Link" entry
         (file+olp
          "~/Notes/denote/20240328T220037--media-tracker__self.org" "Watch/Read List")
	     "* [[%:link][%:description]] \nCaptured On: %U \n%?")
	    ("b" "Blog Article" entry
         (file+olp
          "~/Code/bardmandev/content/_index.org" "Latest updates"))))

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

;; (provide 'bard-emacs-org)
