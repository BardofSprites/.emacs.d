(defun bard/denote-insert-id-at-top ()
  "Insert or replace a top-level :ID: property at the very top of the current file."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   ;; If file already starts with a :PROPERTIES: drawer, remove it
   (when (looking-at ":PROPERTIES:")
     (let ((end (save-excursion
                  (re-search-forward ":END:" nil t))))
       (when end
         (delete-region (point-min) (min (point-max) (1+ end)))))))
  ;; Insert fresh ID drawer at very top
  (goto-char (point-min))
  (let ((id (org-id-new)))
    (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" id)))
  (save-buffer))

(defun denote-sequence-region ()
  "Call `denote-sequence' and insert therein the text of the active region.

Note that, currently, `denote-save-buffers' and
`denote-kill-buffers' are NOT respected.  The buffer is not
saved or killed at the end of `denote-sequence-region'."
  (declare (interactive-only t))
  (interactive)
  (if-let* (((region-active-p))
            ;; We capture the text early, otherwise it will be empty
            ;; the moment `insert' is called.
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      (progn
        (let ((denote-ignore-region-in-denote-command t)
              ;; FIXME: Find a way to insert the region before the buffer is
              ;; saved/killed by the creation command.
              (denote-save-buffers nil)
              (denote-kill-buffers nil))
          (call-interactively 'denote-sequence))
        (push-mark (point))
        (insert text)
        (run-hook-with-args 'denote-region-after-new-note-functions (mark) (point)))
    (call-interactively 'denote-sequence)))

(provide 'bard-writing)
