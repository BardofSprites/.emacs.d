(require 'bard-modeline)

;;; Mode line
(setq mode-line-compact nil) ; Emacs 28
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                prot-modeline-narrow
                bard-modeline-centered-cursor
                prot-modeline-input-method
                bard-modeline-ryo-modal-insert
                bard-modeline-ryo-modal-normal
                prot-modeline-buffer-status
                prot-modeline-window-dedicated-status
                " "
                prot-modeline-buffer-identification
                "  "
                prot-modeline-major-mode
                prot-modeline-process
                "  "
                prot-modeline-vc-branch
                "  "
                prot-modeline-flymake
                prot-modeline-eglot
                "  "
                prot-modeline-notmuch-indicator
                " "
                prot-modeline-misc-info))

(with-eval-after-load 'spacious-padding
  (defun prot/modeline-spacious-indicators ()
    "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
    (if (bound-and-true-p spacious-padding-mode)
        (set-face-attribute 'prot-modeline-indicator-button nil :box t)
      (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified)))

  ;; Run it at startup and then afterwards whenever
  ;; `spacious-padding-mode' is toggled on/off.
  (prot/modeline-spacious-indicators)

  (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators))


(provide 'bard-emacs-modeline)

;;; bard-emacs-modeline.el ends here
