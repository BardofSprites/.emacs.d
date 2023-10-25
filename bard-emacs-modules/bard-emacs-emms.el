;; |------------------------------------|
;; |             EMMS                   |
;; |------------------------------------|

(require 'emms-setup)

(emms-all)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native)
      emms-playlist-buffer-name "*Music*")

;; Keymaps
(define-key global-map (kbd "<f8>") #'emms)
(define-key global-map (kbd "M-<f8>") #'emms-browser)

(provide 'bard-emacs-emms)
