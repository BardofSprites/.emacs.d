(defun bard/play-elfeed-video ()
  "Play the URL of the entry at point in mpv if it's a YouTube video."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (if entry
        (let ((url (elfeed-entry-link entry)))
          (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
              (progn
                (async-shell-command (format "mpv '%s'" url))
                (elfeed-search-untag-all-unread))
            (message "The URL is not a YouTube link: %s" url)))
      (message "No entry selected in Elfeed."))))

(defun bard/add-video-emms-queue ()
  "Play the URL of the entry at point in mpv if it's a YouTube video. Add it to EMMS queue."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (if entry
        (let ((url (elfeed-entry-link entry)))
          (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
              (let* ((playlist-name "Watch Later")
                     (playlist-buffer (get-buffer (format " *%s*" playlist-name))))
                (unless playlist-buffer
                  (setq playlist-buffer (emms-playlist-new (format " *%s*" playlist-name))))
                (emms-playlist-set-playlist-buffer playlist-buffer)
                (emms-add-url url)
                (elfeed-search-untag-all-unread)
                (message "Added YouTube video to EMMS playlist: %s" url))
            (message "The URL is not a YouTube link: %s" url)))
      (message "No entry selected in Elfeed."))))

(provide 'bard-web)
