;;; Vim Bindings
(use-package evil
  :ensure t
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package undo-fu
  :ensure t)

(use-package general
  :ensure t
  :after evil
  :config
  (general-create-definer bard/leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (bard/leader-keys

    ;; files
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")

    ;; buffers
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "bs" '(save-buffer :which-key "save buffer")

    ;; windows
    "w"  '(:ignore t :which-key "windows")
    "ws" '(split-window-below :which-key "split below")
    "wv" '(split-window-right :which-key "split right")
    "wo" '(other-window :which-key "other window")
    ;; "w0" '(delete-window :which-key "delete window")

    ;; projects
    "p" '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "project switch project")
    "pf" '(project-find-file :which-key "project find file")

    ;; notes
    "n" '(:ignore t :which-key "notes")
    "nn" '(denote :which-key "denote note")
    "nd" '(denote-sort-dired :which-key "denote dired")
    "nr" '(denote-rename-file-using-front-matter :which-key "denote rename")
    "nk" '(denote-rename-file-keywords :which-key "denote keywords")
    "nf" '(bard/find-notes-file :which-key "find notes")
    "ng" '(bard/search-notes-directory :which-key "search notes")
    "nN" '(denote-sequence :which-key "sequence note")
    "nD" '(denote-sequence-dired :which-key "sequence dired")
    "ni" '(org-roam-node-insert :which-key "insert link")
    "nl" '(org-roam-buffer-toggle :which-key "org roam buffer")
    "nu" '(org-roam-ui-open :which-key "org roam ui")

    ;; open
    "o" '(:ignore t :which-key "open")
    "oa" '(bard/default-agenda :which-key "agenda")
    "or" '(elfeed :which-key "RSS")
    "ow" '(eww :which-key "web browser")
    "op" '(bard/play-youtube-video :which-key "play video")
    "oi" '(bard/image-browser :which-key "images")
    "oc" '(org-capture :which-key "capture")
    "om" '(notmuch :which-key "email")
    "ob" '(consult-bookmark :which-key "bookmarks")
    "od" '(dired :which-key "new frame")
    "of" '(dired-jump :which-key "dired")

    ;; search
    "s" '(:ignore t :which-key "search")
    "sb" '(consult-line :which-key "search buffer")
    "sf" '(consult-find :which-key "search files")
    "sg" '(consult-grep :which-key "search grep")
    "so" '(consult-outline :which-key "search outline")

    ;; toggle
    "t" '(:ignore t :which-key "toggle")
    "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
    "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
    )
  

  )
(provide 'bard-emacs-keyboard)

