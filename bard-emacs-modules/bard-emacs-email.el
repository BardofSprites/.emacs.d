(require 'bard-email)

(use-package notmuch
  :ensure t
  :config
  (define-key global-map (kbd "C-c m") #'notmuch)
  (setq notmuch-show-logo nil
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-show-all-tags-list t)

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-seen-current-message t)

  (defun bard/notmuch-mua-empty-subject-check ()
    "Request confirmation before sending a message with empty subject."
    (when (and (null (message-field-value "Subject"))
               (not (y-or-n-p "Subject is empty, send anyway? ")))
      (error "Sending message cancelled: empty subject")))
  (add-hook 'message-send-hook 'bard/notmuch-mua-empty-subject-check))

(setq notmuch-show-empty-saved-searches t)
(setq notmuch-saved-searches
      `(( :name "📥 Inbox (all-mail)"
          :query "tag:inbox"
          :sort-order newest-first
          :key ,(kbd "i"))
        ( :name "💬 Unread (inbox)"
          :query "tag:unread and tag:inbox"
          :sort-order newest-first
          :key ,(kbd "u"))
        ( :name "🚩 Flagged"
          :query "tag:flag"
          :sort-order newest-first
          :key ,(kbd "f"))
        ( :name "📚 Archive"
          :query "tag:archive"
          :sort-order oldest-first
          :key ,(kbd "a"))
        ( :name "📜 Reference"
          :query "tag:ref"
          :sort-order oldest-first
          :key ,(kbd "r"))
        ( :name "📬 Mailing Lists"
          :query "tag:list and tag:unread"
          :sort-order oldest-first
          :key ,(kbd "l"))
        ( :name "🎨 Emacs - Humanities"
          :query "tag:unread and tag:humanities"
          :sort-order newest-first
          :key ,(kbd "lh"))
        ( :name "🦄 Emacs - Org Mode"
          :query "tag:unread and tag:org"
          :sort-order newest-first
          :key ,(kbd "lo"))))

(setq notmuch-tagging-keys
      `((,(kbd "d") bard-notmuch-mark-delete-tags "💥 Mark for deletion")
        (,(kbd "f") bard-notmuch-mark-flag-tags "🚩 Flag as important")
        (,(kbd "s") bard-notmuch-mark-spam-tags "🔥 Mark as spam")
        (,(kbd "R") bard-notmuch-mark-ref-tags "📜 Mark for reference")
        (,(kbd "r") ("-unread") "👁️‍ Mark as read")
        (,(kbd "u") ("+unread") "🗨️ Mark as unread")))

(setq notmuch-archive-tags '("+archive" "-unread" "-inbox")
      notmuch-message-replied-tags '("+replied")
      notmuch-message-forwarded-tags '("+forwarded")
      notmuch-show-mark-read-tags '("-unread")
      notmuch-draft-tags '("+draft")
      notmuch-draft-folder "drafts"
      notmuch-draft-save-plaintext 'ask)

(use-package notmuch-indicator
  :ensure t
  :after notmuch
  :config
  (setq notmuch-indicator-args
        '(( :terms "tag:account/home and tag:inbox and tag:unread"
            :label "[Дом] "
            ;; :label-face prot-modeline-indicator-green
            ;; :counter-face prot-modeline-indicator-green
            )
          ( :terms "tag:account/devel and tag:inbox and tag:unread"
            :label "[Прог] "
            ;; :label-face prot-modeline-indicator-cyan
            ;; :counter-face prot-modeline-indicator-cyan
            )
          ( :terms "tag:inbox"
            :label "[Все] "
            ;; :label-face prot-modeline-indicator-blue
            ;; :counter-face prot-modeline-indicator-blue
            ))

        notmuch-indicator-refresh-count (* 60 3)
        notmuch-indicator-hide-empty-counters nil
        notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))
  (setq notmuch-indicator-add-to-mode-line-misc-info nil)
  (notmuch-indicator-mode t))

(use-package ol-notmuch
  :ensure t
  :after notmuch)

;; use msmtp
(setq sendmail-program "/usr/bin/msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil nil
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from t)

(provide 'bard-emacs-email)
