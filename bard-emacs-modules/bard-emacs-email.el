(require 'bard-email)

(use-package notmuch
  :ensure t
  :config
  (define-key global-map (kbd "C-c m") #'notmuch)
  (setq notmuch-show-logo t
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-alltags)
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
      `(( :name "📥 inbox (all-mail)"
          :query "tag:inbox"
          :sort-order newest-first
          :key ,(kbd "i"))
        ( :name "💬 unread (inbox)"
          :query "tag:unread and tag:inbox"
          :sort-order newest-first
          :key ,(kbd "u"))
        ( :name "📝 To Do"
          :query "tag:todo"
          :sort-order oldest-first
          :key ,(kbd "t"))
        ( :name "🚩 flagged"
          :query "tag:flag"
          :sort-order newest-first
          :key ,(kbd "f"))
        ( :name "🐃 contributions"
          :query "tag:unread and tag:contrib"
          :sort-order newest-first
          :key ,(kbd "c"))
        ( :name "🐧 linux-related"
          :query "tag:unread and tag:linux"
          :sort-order newest-first
          :key ,(kbd "l"))
        ( :name "🚂 emacs developement"
          :query "tag:unread and tag:contrib"
          :sort-order newest-first
          :key ,(kbd "ed"))
        ( :name "🎨 emacs humanities"
          :query "tag:unread and tag:emacs-humanities"
          :sort-order newest-first
          :key ,(kbd "eh"))
        ( :name "🦄 emacs org-mode"
          :query "tag:unread and tag:emacs-org"
          :sort-order newest-first
          :key ,(kbd "eo"))))

(setq notmuch-tagging-keys
      `((,(kbd "d") prot-notmuch-mark-delete-tags "💥 Mark for deletion")
        (,(kbd "f") prot-notmuch-mark-flag-tags "🚩 Flag as important")
        (,(kbd "s") prot-notmuch-mark-spam-tags "🔥 Mark as spam")
        (,(kbd "r") ("-unread") "👁️‍🗨️ Mark as read")
        (,(kbd "u") ("+unread") "🗨️ Mark as unread")))

(setq notmuch-archive-tags '("+archive")
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
        '(( :terms "tag:unread and tag:inbox"
            :label "[U] "
            :label-face prot-modeline-indicator-green
            :counter-face prot-modeline-indicator-green)
          ( :terms "tag:unread and tag:linux"
            :label "[L] "
            :label-face prot-modeline-indicator-cyan
            :counter-face prot-modeline-indicator-cyan)
          ( :terms "tag:unread and tag:emacs"
            :label "[E] "
            :label-face prot-modeline-indicator-blue
            :counter-face prot-modeline-indicator-blue))

        notmuch-indicator-refresh-count (* 60 3)
        notmuch-indicator-hide-empty-counters t
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
