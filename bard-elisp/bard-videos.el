(require 'cl-lib)
(require 'seq)

(defun bard/play-youtube-video ()
  "Prompt for a YouTube URL and play it in mpv."
  (interactive)
  (let ((url (read-string "Enter YouTube URL: ")))
    (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
        (async-shell-command (format "mpv '%s'" url))
      (message "The URL is not a valid YouTube link: %s" url))))

;; (defun bard/add-mpv-log-video-emms-queue (file)
;;   "Play the URL or file path of the entry at point in mpv log file. Add it to EMMS queue."
;;   (interactive "fLog file: ")
;;   (let ((history '())        ;; Store video candidates
;;         (max-candidates 10))  ;; Limit to the most recent 10 candidates
;;     (with-temp-buffer
;;       (insert-file-contents file)
;;       (goto-char (point-min))
;;       ;; Loop through each line of the log
;;       (while (re-search-forward
;;               "^\\[.*\\] \\(.*\\)\\(?: \\|$\\)" nil t)
;;         (let ((candidate (match-string 1)))
;;           ;; If it's a URL or file path, add it to the history list
;;           (when (or (string-match-p "https?://" candidate)
;;                     (file-exists-p candidate))
;;             (push candidate history))))
;;       ;; Remove duplicates
;;       (setq history (delete-dups history))
;;       ;; Limit the history to the most recent ones
;;       (setq history (cl-subseq history 0 (min max-candidates (length history))))
;;       ;; Select the most recent URL or file path
;;       (if history
;;           (let* ((chosen (completing-read "Select video: " history nil t)))
;;             (if (string-match-p "https?://" chosen)
;;                 (let* ((playlist-name "Watch Later")
;;                        (playlist-buffer (get-buffer (format " *%s*" playlist-name))))
;;                   (unless playlist-buffer
;;                     (setq playlist-buffer (emms-playlist-new (format " *%s*" playlist-name))))
;;                   (emms-playlist-set-playlist-buffer playlist-buffer)
;;                   (emms-add-url chosen)
;;                   (message "Added YouTube video to EMMS playlist: %s" chosen))
;;               (let* ((playlist-name "Watch Later")
;;                      (playlist-buffer (get-buffer (format " *%s*" playlist-name))))
;;                 (unless playlist-buffer
;;                   (setq playlist-buffer (emms-playlist-new (format " *%s*" playlist-name))))
;;                 (emms-playlist-set-playlist-buffer playlist-buffer)
;;                 (emms-add-file chosen)
;;                 (message "Added video file to EMMS playlist: %s" chosen)))
;;             (message "No recent videos found in the log."))))))



;; (defun bard/classify-content (input)
;;   "Classify INPUT as a URL, a file path, or neither."
;;   (cond
;;    ((let ((parsed-url (url-generic-parse-url input)))
;;       (and (url-type parsed-url)
;;            (url-host parsed-url)))
;;     'url)

;;    ((file-name-absolute-p input)
;;     (if (file-exists-p input)
;;         'existing-file
;;       'potential-file))
;;    (t 'neither)))

;; (defun bard/play-video-url (url)
;;   (let* ((playlist-name "Watch Later")
;;          (playlist-buffer (get-buffer (format " *%s*" playlist-name))))
;;     (unless playlist-buffer
;;       (setq playlist-buffer (emms-playlist-new (format " *%s*" playlist-name))))
;;     (emms-playlist-set-playlist-buffer playlist-buffer)
;;     (emms-add-url url)
;;     (message "Added YouTube video to EMMS playlist: %s" url)))

;; (defun bard/play-video-file (file)
;;   (let* ((playlist-name "Watch Later")
;;          (playlist-buffer (get-buffer (format " *%s*" playlist-name))))
;;     (unless playlist-buffer
;;       (setq playlist-buffer (emms-playlist-new (format " *%s*" playlist-name))))
;;     (emms-playlist-set-playlist-buffer playlist-buffer)
;;     (emms-add-file url)
;;     (message "Added video file to EMMS playlist: %s" url)))

;; ;; TODO: extract url/filepath into bard/classify-content
;; ;; then to case/switch with symbols returned by ^^^ to determine which play function to use (both written above)
;; (defun bard/play-vid-from-history (history-file)
;;   "Read the mpv log HISTORY-FILE, and let the user select and play a video."
;;   (interactive "fSelect MPV log file: ")
;;   (let ((history '())
;;         (max-candidates 10))
;;     (with-temp-buffer
;;       (insert-file-contents history-file)
;;       (goto-char (point-min))
;;       ;; Loop through each line of the log
;;       (while (not (eobp))
;;         (let* ((log-line (buffer-substring-no-properties
;;                           (line-beginning-position) (line-end-position)))
;;                )
;;           (push log-line history)
;;           ;; Add extracted content to the history
;;         (forward-line 1))
;;       ;; Remove duplicates and limit the history size
;;       (setq history (delete-dups history))
;;       (setq history (cl-subseq history 0 (min max-candidates (length history)))))
;;     ;; Select an item from the history
;;     (if history
;;         (let* ((chosen (completing-read "Select video: " history nil t))
;;                (content-type (bard/classify-content chosen)))
;;           ;; Handle different content types
;;           (extract-file-path chosen))
;;       (error "No valid history found")))))

;; (setq bard/mpv-history-log "~/.config/mpv/mpvHistory.log")
;; (bard/play-vid-from-history bard/mpv-history-log)

(provide 'bard-videos.el)
