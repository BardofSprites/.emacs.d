(require 'bard-ryo)

(use-package hydra
  :ensure t
  :config
  )

(use-package ryo-modal
  :ensure t
  :bind (("C-c SPC" . ryo-modal-mode)
         ("<escape>" . ryo-modal-mode))
  :config
  (setq-default cursor-type '(bar . 2))
  (setq ryo-modal-cursor-type 'box)
  (ryo-modal-keys
   ("," ryo-modal-repeat)
   ("f" ryo-modal-mode)
   ;; movement
   ("i" previous-line)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)
   ;; word movement
   ("o" forward-word)
   ("u" backward-word)

   ;; sexp movement
   ("(" backward-sexp)
   (")" forward-sexp)

   ;; deletion
   ("e" backward-kill-word)
   ("r" kill-word)
   ("d" backward-delete-char)

   ;; copy/paste/cut
   ("x" kill-region)
   ("c" kill-ring-save)
   ("v" yank)

   ("s" open-line)
   ("y" undo)
   ("t" set-mark-command)

   ;; searching
   ("n" isearch-forward)
   ("N" isearch-backward)

   ;; command mode
   ("a" "M-x")

   ;;window management
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right))

  (define-key global-map (kbd "C-<SPC>") nil)

  (ryo-modal-key
   "SPC"
   '(("SPC" ryo-modal-mode)
     ("w" save-buffer)
     ("j" dired-jump)
     ("f" find-file)
     ("m" pop-to-mark-command)))

  (ryo-modal-key
   "SPC r" :hydra
   '(hydra-replace ()
                      "Replace/Substitute hydra"
                      ("a" substitute-target-above-point "above")
                      ("b" substitute-target-below-point "below")
                      ("d" substitute-target-in-defun "defun")
                      ("s" substitute-target-in-buffer "buffer")
                      ("r" query-replace-regexp "regexp"))
   :norepeat t)

  (ryo-modal-key
   "SPC n" :hydra
   '(hydra-denote ()
                  "Denote hydra"
                  ("n" denote "new note")
                  ("<SPC>" denote-region "note region")
                  ("o" denote-sort-dired "sort dired")
                  ("j" denote-journal-extras-new-entry "journal")
                  ("r" denote-rename-file-using-front-matter "rename note")
                  ("k" denote-rename-file-keywords "edit keywords")
                  ("i" denote-link "insert link")
                  ("I" denote-add-links "insert listed links")
                  ("b" denote-backlinks "list backlinks")
                  ("f" bard/find-notes-file "find in notes")
                  ("s" bard/search-notes-directory "search in notes")
                  ("l" denote-find-link "list links")
                  ("L" denote-find-backlink "list backlinks")
                  ("q" nil "cancel" :color red)
                  )
   :norepeat t)

  (ryo-modal-key
   "SPC s" :hydra
   '(hydra-consult ()
                  "Consult hydra"
                  ("f" consult-find "find")
                  ("g" consult-grep "grep")
                  ("k" consult-kmacro "kmacro")
                  ("o" consult-outline "outline")
                  ("r" consult-register "register")
                  ("l" consult-line "search file")
                  ("q" nil "cancel" :color red))
   :norepeat t)

  (ryo-modal-key
   "SPC o" :hydra
   '(hydra-open ()
                "Open hydra"
                ("a" bard/default-agenda "agenda")
                ("c" calendar "calendar")
                ("g" magit-status "git")
                ("m" notmuch "mail")
                ("q" nil "cancel" :color red)
                ("r" elfeed "rss")
                ("w" eww "web browser")
                ("x" org-capture "capture"))
   :norepeat t)

  (ryo-modal-key
   "SPC t" :hydra
   '(hydra-tab ()
               "Tab hydra"
               ("0" tab-close "close")
               ("1" tab-close-other "delete others")
               ("2" tab-new "new")
               ("t" tab-next "next")
               ("T" tab-next "prev")
               ("d" dired-other-tab "dired tab")
               ("q" nil "cancel" :color red))
   :norepeat t)

  (ryo-modal-key
   "SPC b" :hydra
   '(hydra-buffer ()
               "Buffer hydra"
               ("k" kill-buffer "kill buffer")
               ("b" consult-buffer "switch-buffer")
               ("l" ibuffer "buffer list")
               ("q" nil "cancel" :color red))
   :norepeat t)

  )

(provide 'bard-emacs-keyboard)
