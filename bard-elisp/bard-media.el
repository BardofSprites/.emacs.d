(require 'cl-lib)
(require 'seq)
(require 'emms)
(require 'image-dired)
(require 'dired-x)

(defun bard/play-youtube-video ()
  "Play the YouTube URL at point or prompt for one if none is found."
  (interactive)
  (let* ((url-at-point (thing-at-point 'url t))
         (url (if (and url-at-point
                       (string-match-p "https?://\\(www\\.\\)?\\(youtube\\.com\\|youtu\\.be\\)" url-at-point))
                  url-at-point
                (read-string "Enter YouTube URL: "))))
    (if (and url (string-match-p "https?://\\(www\\.\\)?\\(youtube\\.com\\|youtu\\.be\\)" url))
        (async-shell-command (format "mpv '%s'" url))
      (message "The URL is not a valid YouTube link: %s" url))))

(defun bard/save-emms-watch-later ()
  "Save the current EMMS playlist to `bard/watch-later-file` using `bard/emms-playlist-format`."
  (interactive)
  (when (and bard/watch-later-file bard/emms-playlist-format)
    (emms-playlist-save bard/emms-playlist-format bard/watch-later-file)
    (message "Playlist saved to %s" bard/watch-later-file)))

(defun bard/image-browser-choose (directory)
  "Open nsxiv in thumbnail mode on DIRECTORY.
Asks the user whether to enable recursive mode."
  (interactive "DSelect directory: ")
  (let ((recursive (if (y-or-n-p "Recursive searching? ") "-r" ""))
        (stdout (if (y-or-n-p "Output marked files to buffer? ") "-o" "")))
    (let ((process (start-process "nsxiv" "*nsxiv*" "nsxiv" "-t" stdout recursive (expand-file-name directory))))
      (when (string= stdout "-o")
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (string= event "finished\n")
             (with-current-buffer "*nsxiv*"
               (read-only-mode nil)
               (erase-buffer)))))
        (pop-to-buffer "*nsxiv*")))))

(defun bard/image-browser-marked ()
  "Open nsxiv on the marked files in Dired."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (apply #'start-process "nsxiv" "*nsxiv*" "nsxiv" "-t" files)
      (message "No files marked."))))

(defun bard/image-browser ()
  "Open nsxiv in a context-sensitive way:
- If in Dired with marked files, open those with nsxiv.
- Otherwise, prompt for a directory to browse."
  (interactive)
  (if (and (derived-mode-p 'dired-mode)
           (dired-get-marked-files))
      (bard/image-browser-marked)
    (call-interactively #'bard/image-browser-choose)))

(defun bard/emms-download-current-video (destination)
  "Download the currently playing EMMS video and move it to DESTINATION."
  (interactive "DSelect destination directory: ")
  (require 'emms)
  (let* ((track (emms-playlist-current-selected-track))
         (url (emms-track-get track 'name))
         (default-directory (file-name-as-directory temporary-file-directory))
         (downloader (executable-find "yt-dlp"))
         (output-template "%(title)s.%(ext)s"))
    (unless downloader
      (error "yt-dlp or youtube-dl is not installed or not in PATH"))
    (unless (string-match-p "^https?://" url)
      (error "Current track is not a valid video URL"))

    (let ((cmd (format "%s -f best -o \"%s\" \"%s\""
                       downloader output-template url)))
      (message "Downloading video from: %s" url)
      (let ((exit-code (shell-command cmd)))
        (if (not (eq exit-code 0))
            (error "Download failed, see *Messages* for details")
          ;; Move the downloaded file
          (let* ((downloaded-file (car (directory-files default-directory t ".*\\(mp4\\|mkv\\|webm\\)$" 'time)))
                 (target-path (expand-file-name (file-name-nondirectory downloaded-file) destination)))
            (rename-file downloaded-file target-path t)
            (message "Video saved to: %s" target-path)))))))

(provide 'bard-media.el)
