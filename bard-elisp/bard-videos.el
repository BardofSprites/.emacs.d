(require 'cl-lib)
(require 'seq)
(require 'emms)

(defun bard/play-youtube-video ()
  "Prompt for a YouTube URL and play it in mpv."
  (interactive)
  (let ((url (read-string "Enter YouTube URL: ")))
    (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
        (async-shell-command (format "mpv '%s'" url))
      (message "The URL is not a valid YouTube link: %s" url))))

(defun bard/save-emms-watch-later ()
  "Save the current EMMS playlist to `bard/watch-later-file` using `bard/emms-playlist-format`."
  (interactive)
  (when (and bard/watch-later-file bard/emms-playlist-format)
    (emms-playlist-save bard/emms-playlist-format bard/watch-later-file)
    (message "Playlist saved to %s" bard/watch-later-file)))

(provide 'bard-videos.el)
