;; |------------------------------------|
;; |            Gen Config              |
;; |------------------------------------|
;; Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Fonts
(set-face-attribute 'default nil :font "Iosevka Comfy" :height 140)
(set-face-attribute 'variable-pitch nil :font "Iosevka Comfy Wide" :height 140)

;; No Backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq confirm-kill-emacs 'y-or-n-p)

;; |------------------------------------|
;; |             UI Config              |
;; |------------------------------------|
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(display-line-numbers-mode t)
(setq display-line-numbers 'relative)

;; |------------------------------------|
;; |          Modes and Hooks           |
;; |------------------------------------|
;; pair parens and quotes automatically
(electric-pair-mode 1)

;; |------------------------------------|
;; |             Packages               |
;; |------------------------------------|

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Ef-themes
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-day t)
  (define-key global-map (kbd "<f5>") #'ef-themes-toggle)
  (setq ef-themes-to-toggle '(ef-autumn ef-day))
  (setq ef-themes-headings
      '((0 variable-pitch 1.8)
        (1 variable-pitch 1.3)
        (2 regular 1.2)
        (3 1.1)
        (agenda-structure variable-pitch 1.5)
        (t variable-pitch))))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

;; Vertico completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; Marginalia - works with vertico
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode 1))

;; |------------------------------------|
;; |            Keybinds                |
;; |------------------------------------|

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Smex
;;(global-set-key (kbd "M-x") 'smex)

;; |------------------------------------|
;; |            Org Config              |
;; |------------------------------------|

(setq org-directory "~/Notes/Org-Roam/")
(setq org-agenda-files (list "~/Notes/Org-Roam/todo.org"))
(setq org-agenda-files (list "~/Notes/Org-Roam/todo.org"))

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "All Tasks \n")))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-start-day nil)
                      (org-deadline-warning-days 0)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "Today's agenda \n")))
          ;; write skip function that skips saturdays and sundays
          (agenda "" ((org-agenda-span 7)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this week \n")))))
        ("Y" "Monthly view for all tasks"
         ((agenda "" ((org-agenda-span 365)
                      (org-deadline-warning-days 2)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this Year\n")))))
        ("S" "Monthly view for all tasks"
         ((agenda "" ((org-agenda-span 31)
                      (org-deadline-warning-days 2)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "Upcoming this month\n")))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(marginalia company vertico magit vterm use-package multiple-cursors ef-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
