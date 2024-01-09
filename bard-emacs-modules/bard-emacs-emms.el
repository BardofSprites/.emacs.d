;; |------------------------------------|
;; |             EMMS                   |
;; |------------------------------------|

(require 'emms-info-mediainfo)
(add-to-list 'emms-info-functions 'emms-info-mediainfo)

(emms-all)
(emms-default-players)
(emms-mpris-enable)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))

(setq emms-browser-covers 'emms-browser-cache-thumbnail)

;; Keymaps
(define-key global-map (kbd "<f8>") #'emms)
(define-key global-map (kbd "M-<f8>") #'emms-browser)
(define-key emms-playlist-mode-map (kbd "A") #'emms-add-directory)
(define-key emms-playlist-mode-map (kbd "T") #'emms-add-directory-tree)
(define-key emms-playlist-mode-map (kbd "L") #'emms-toggle-repeat-track)
(define-key emms-playlist-mode-map (kbd "<mouse-3>") #'emms-pause)

(provide 'bard-emacs-emms)
