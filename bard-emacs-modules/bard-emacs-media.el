;;;; EMMS
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
        ("N" . emms-playlist-mode-shift-track-down)
        ("Z" . bard/save-emms-watch-later)
        ("Y" . bard/emms-download-current-video))
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

  ;; center line function
  (defun bard/emms-recenter ()
    (interactive)
    (recenter-top-bottom)
    (emms-playlist-mode-center-current))

  ;; modeline
  (emms-mode-line-disable)
  (emms-playing-time-disable-display)

  ;; playlist saving
  (setq bard/emms-playlist-format 'm3u)
  (setq bard/watch-later-file "~/Videos/watch-later.m3u")
  )

;;;; Youtube
(use-package yeetube
  :ensure t
  )

;;;; PDFS
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (add-to-list 'pdf-tools-enabled-modes #'pdf-view-themed-minor-mode))

(use-package bard-media
  :ensure nil
  :config
  (require 'bard-media)
  :bind
  (("C-c o p" . bard/play-youtube-video)
   ("C-c o i" . bard/image-browser))
  )

(provide 'bard-emacs-media)
