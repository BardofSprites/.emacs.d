;; General `notmuch' ui configuration

(use-package notmuch
  :ensure t
  :config
  (define-key global-map (kbd "C-c m") #'notmuch))

(use-package notmuch-indicator)

(use-package mbsync)

(setq notmuch-show-logo nil
      notmuch-column-control 1.0
      notmuch-hello-auto-refresh t
      notmuch-hello-recent-searches-max 20
      notmuch-hello-thousands-separator ""
      notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
      notmuch-show-all-tags-list t)

(setq notmuch-search-oldest-first nil)
(setq notmuch-show-seen-current-message t)

(defun bard/notmuch-mua-empty-subject-check ()
  "Request confirmation before sending a message with empty subject."
  (when (and (null (message-field-value "Subject"))
             (not (y-or-n-p "Subject is empty, send anyway? ")))
    (error "Sending message cancelled: empty subject")))
(add-hook 'message-send-hook 'bard/notmuch-mua-empty-subject-check)

(setq notmuch-saved-searches
        `(( :name "📥 inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "💬 unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))))


(setq notmuch-archive-tags '("+archive")
      notmuch-message-replied-tags '("+replied")
      notmuch-message-forwarded-tags '("+forwarded")
      notmuch-show-mark-read-tags '("-unread")
      notmuch-draft-tags '("+draft")
      notmuch-draft-folder "drafts"
      notmuch-draft-save-plaintext 'ask)
