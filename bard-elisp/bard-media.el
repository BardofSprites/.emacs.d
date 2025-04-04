(require 'cl-lib)
(require 'seq)
(require 'emms)
(require 'image-dired)
(require 'dired-x)

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

;; (defun bard/image-browser (directory)
;;   "Open nsxiv in thumbnail mode on DIRECTORY.
;; Asks the user whether to enable recursive mode."
;;   (interactive "DSelect directory: ")
;;   (let ((recursive (if (y-or-n-p "Recursive searching? ") "-r" "")))
;;     (start-process "nsxiv"  "nsxiv" "-t" "-o" recursive (expand-file-name directory))))

;; (defun bard/image-browser (directory)
;;   "Open nsxiv in thumbnail mode on DIRECTORY.
;; Asks the user whether to enable recursive mode."
;;   (interactive "DSelect directory: ")
;;   (let ((recursive (if (y-or-n-p "Recursive searching? ") "-r" "")))
;;     (start-process "nsxiv" "*nsxiv*" "nsxiv" "-t" "-o" recursive (expand-file-name directory))
;;     (pop-to-buffer "*nsxiv*")))

;; (defun bard/image-browser (directory)
;;   "Open nsxiv in thumbnail mode on DIRECTORY.
;; Asks the user whether to enable recursive mode."
;;   (interactive "DSelect directory: ")
;;   (let ((recursive (if (y-or-n-p "Recursive searching? ") "-r" ""))
;;         (stdout (if (y-or-n-p "Output marked files to buffer? ") "-o" "")))
;;     (start-process "nsxiv" "*nsxiv*" "nsxiv" "-t" stdout recursive (expand-file-name directory))
;;     (if (string= stdout "-o")
;;         (progn (with-current-buffer "*nsxiv*"
;;                  (read-only-mode nil)
;;                  (erase-buffer))
;;                (pop-to-buffer "*nsxiv*")
;;                )
;;       nil)))

(defun bard/image-browser (directory)
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

(provide 'bard-media.el)
