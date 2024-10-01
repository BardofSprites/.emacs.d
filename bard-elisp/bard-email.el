(require 'prot-common)
(eval-when-compile (require 'cl-lib))

(defgroup prot-notmuch ()
  "Extensions for notmuch.el."
  :group 'notmuch)

(defcustom prot-notmuch-delete-tag "del"
  "Single tag that applies to mail marked for deletion.
This is used by `prot-notmuch-delete-mail'."
  :type 'string
  :group 'prot-notmuch)

(defcustom prot-notmuch-mark-delete-tags
  `(,(format "+%s" prot-notmuch-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion.
To actually delete email, refer to `prot-notmuch-delete-mail'."
  :type '(repeat string)
  :group 'prot-notmuch)

(defcustom prot-notmuch-mark-flag-tags '("+flag" "-unread")
  "List of tags to mark as important (flagged).
This gets the `notmuch-tag-flagged' face, if that is specified in
`notmuch-tag-formats'."
  :type '(repeat string)
  :group 'prot-notmuch)

(defcustom prot-notmuch-mark-spam-tags '("+spam" "-inbox" "-unread")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'prot-notmuch)

(autoload 'notmuch-interactive-region "notmuch")
(autoload 'notmuch-tag-change-list "notmuch")
(autoload 'notmuch-search-next-thread "notmuch")
(autoload 'notmuch-search-tag "notmuch")

(defmacro prot-notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.

Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags.

This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (when ,tags
       (notmuch-search-tag
        (notmuch-tag-change-list ,tags untag) beg end))
     (when (eq beg end)
       (notmuch-search-next-thread))))

(prot-notmuch-search-tag-thread
  prot-notmuch-search-delete-thread
  prot-notmuch-mark-delete-tags)

(prot-notmuch-search-tag-thread
  prot-notmuch-search-flag-thread
  prot-notmuch-mark-flag-tags)

(prot-notmuch-search-tag-thread
  prot-notmuch-search-spam-thread
  prot-notmuch-mark-spam-tags)

(defmacro prot-notmuch-show-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
	          (notmuch-tag-change-list ,tags untag)))))

(prot-notmuch-show-tag-message
  prot-notmuch-show-delete-message
  prot-notmuch-mark-delete-tags)

(prot-notmuch-show-tag-message
  prot-notmuch-show-flag-message
  prot-notmuch-mark-flag-tags)

(prot-notmuch-show-tag-message
  prot-notmuch-show-spam-message
  prot-notmuch-mark-spam-tags)

(defun prot-notmuch-delete-mail ()
  "Permanently delete mail marked as `prot-notmuch-delete-mail'.
Prompt for confirmation before carrying out the operation.

Do not attempt to refresh the index.  This will be done upon the
next invocation of 'notmuch new'."
  (interactive)
  (let* ((del-tag prot-notmuch-delete-tag)
         (count
          (string-to-number
           (with-temp-buffer
             (shell-command
              (format "notmuch count tag:%s" prot-notmuch-delete-tag) t)
             (buffer-substring-no-properties (point-min) (1- (point-max))))))
         (mail (if (> count 1) "mails" "mail")))
    (unless (> count 0)
      (user-error "No mail marked as `%s'" del-tag))
    (when (yes-or-no-p
           (format "Delete %d %s marked as `%s'?" count mail del-tag))
      (shell-command
       (format "notmuch search --output=files --format=text0 tag:%s | xargs -r0 rm" del-tag)
       t))))
(provide 'bard-email)
