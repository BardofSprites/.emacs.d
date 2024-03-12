;; |------------------------------------|
;; |             EMMS                   |
;; |------------------------------------|

(require 'emms)
(require 'emms-setup)
(require 'emms-mpris)
(require 'emms-mode-line-icon)

(emms-all)
(emms-default-players)
(emms-mpris-enable)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))

(setq emms-browser-covers 'emms-browser-cache-thumbnail)

(setq emms-volume-amixer-card 1)

;; center line function
(defun bard/emms-recenter ()
  (interactive)
  (recenter-top-bottom)
  (emms-playlist-mode-center-current))

;; modeline
(emms-mode-line-disable)
(emms-playing-time-disable-display)

;; Keymaps
(define-key global-map (kbd "<f8>") #'emms)
(define-key global-map (kbd "M-<f8>") #'emms-browser)
(define-key emms-playlist-mode-map (kbd "A") #'emms-add-directory)
(define-key emms-playlist-mode-map (kbd "T") #'emms-add-directory-tree)
(define-key emms-playlist-mode-map (kbd "F") #'emms-add-file)
(define-key emms-playlist-mode-map (kbd "L") #'emms-toggle-repeat-track)
(define-key emms-playlist-mode-map (kbd "<mouse-3>") #'emms-pause)
(define-key emms-playlist-mode-map (kbd "<SPC>") #'emms-pause)
(define-key emms-playlist-mode-map (kbd "c") #'bard/emms-recenter)

(provide 'bard-emacs-emms)
