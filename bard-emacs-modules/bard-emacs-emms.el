;; |------------------------------------|
;; |             EMMS                   |
;; |------------------------------------|

(require 'emms-setup)
(require 'emms-mpris)

(emms-all)
(emms-default-players)
(emms-mpris-enable)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native)
      emms-playlist-buffer-name "*Music*")

;; Keymaps
(define-key global-map (kbd "<f8>") #'emms)
(define-key global-map (kbd "M-<f8>") #'emms-browser)
(define-key emms-playlist-mode-map (kbd "A") #'emms-add-directory)

(provide 'bard-emacs-emms)
