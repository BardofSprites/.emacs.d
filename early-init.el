(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

(setq mode-line-misc-info
      (delete (assoc 'minor-mode-alist mode-line-misc-info) mode-line-misc-info))

;; Modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(tool-bar-mode 0)

;; settings for windows
(setq focus-follows-mouse t)

(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defvar bard-emacs--file-name-handler-alist file-name-handler-alist)
(defvar bard-emacs--vc-handled-backends vc-handled-backends)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist bard-emacs--file-name-handler-alist
                  vc-handled-backends bard-emacs--vc-handled-backends)))

;; Package cache
(setq package-enable-at-startup t)
