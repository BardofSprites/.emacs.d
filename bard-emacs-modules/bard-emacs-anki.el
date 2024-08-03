(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("C-c M-i" . bard/anki-editor-cloze-region-auto-incr)
              ("C-c M-I" . bard/anki-editor-cloze-region-dont-incr)
              ("C-c M-r" . bard/anki-editor-reset-cloze-number))

  :hook (org-capture-after-finalize . bard/anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun bard/anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun bard/anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun bard/anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  ;; Initialize
  (bard/anki-editor-reset-cloze-number)
  )
