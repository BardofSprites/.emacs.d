;; |------------------------------------|
;; |             EMMS                   |
;; |------------------------------------|

;; (require 'emms)
;; (require 'emms-setup)
;; (require 'emms-mpris)
;; (require 'emms-mode-line-icon)

(use-package emms
  :ensure t
  ;; :demand t
  :bind
  (:map emms-playlist-mode-map
        ("A" . emms-add-directory)
        ("l" . emms-add-playlist)
        ("T" . emms-add-directory-tree)
        ("F" . emms-add-file)
        ("U" . emms-add-url)
        ("L" . emms-toggle-repeat-track)
        ("<mouse-3>" . emms-pause)
        ("<SPC>" . emms-pause)
        ("c" . bard/emms-recenter)
        ("P" . emms-playlist-mode-shift-track-up)
        ("N" . emms-playlist-mode-shift-track-down))
  :bind (("<f8>" . emms)
         ("M-<f8>" . emms-browser))
  :hook
  (emms-playlist-mode . hl-line-mode)
  :config
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  (setq emms-player-list '(emms-player-mpv))
  ;; emms-info-functions '(emms-info-native)

  ;; (setq emms-browser-covers 'emms-browser-cache-thumbnail)

  (setq emms-volume-amixer-card 0)

  (require 'emms-player-simple)
  (define-emms-simple-player mpv-video '(file url)
    (regexp-opt '("mp4" "mkv" "webm" "youtube.com" "youtu.be"))
    "mpv" "--quiet" "--no-audio-display" "--vid=1")
  (add-to-list 'emms-player-list 'emms-player-mpv-video)

  ;; center line function
  (defun bard/emms-recenter ()
    (interactive)
    (recenter-top-bottom)
    (emms-playlist-mode-center-current))

  ;; modeline
  (emms-mode-line-disable)
  (emms-playing-time-disable-display)
  )

(provide 'bard-emacs-emms)
