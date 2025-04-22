(require 'emms)
(require 'elfeed-search)

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

(defun bard/add-video-watch-later ()
  "Add the current Elfeed YouTube entry URL to '~/Videos/watch-later.m3u' and mark it as read."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (if entry
        (let* ((url (elfeed-entry-link entry))
               (watch-later-file (expand-file-name "~/Videos/watch-later.m3u")))
          (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
              (progn
                (with-temp-buffer
                  (insert (concat url "\n"))
                  (append-to-file (point-min) (point-max) watch-later-file))
                ;; Remove the 'unread tag from the entry directly
                (setf (elfeed-entry-tags entry)
                      (remove 'unread (elfeed-entry-tags entry)))
                ;; Force UI update
                (when (derived-mode-p 'elfeed-search-mode)
                  (elfeed-search-update-entry entry))
                (message "Added video to watch later: %s" url))
            (message "The URL is not a YouTube link: %s" url)))
      (message "No entry selected in Elfeed."))))

(provide 'bard-web)
