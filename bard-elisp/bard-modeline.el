;;; bard-modeline.el --- prot-style modeline colored via modus/doric-themes -*- lexical-binding: t -*-

;; This is a variant of the reference `bard-modeline'/`prot-modeline'
;; library.  The only substantive change is in the "Faces" section:
;; instead of hardcoding hex colors in each `defface', the indicator
;; faces are populated at run time from whichever theme palette is
;; active -- either `modus-themes' or `doric-themes' -- and are kept
;; in sync automatically whenever you switch or reload a theme.
;;
;; Everything else (helper functions, mode line constructs, risky
;; local variables) is unchanged from the reference file.

(require 'prot-common)

(defgroup prot-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup prot-modeline-faces nil
  "Faces for my custom modeline."
  :group 'prot-modeline)

(defcustom prot-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
        (and (minibuffer-window-active-p (minibuffer-window))
             (with-selected-window (minibuffer-window)
               (eq window (minibuffer-selected-window)))))))

;;;; Faces
;;
;; Rather than hardcoding hex values, these faces are left with an
;; empty spec and are populated/refreshed by `bard-modeline-setup-faces',
;; which reads colors from whichever theme package is currently
;; active.  See "Theme-aware face setup" further below.

(defface prot-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defface prot-modeline-indicator-red
  '((default :inherit bold))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-red-bg
  '((default :inherit (bold prot-modeline-indicator-button)))
  "Face for modeline indicators with a background.
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-green
  '((default :inherit bold))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-green-bg
  '((default :inherit (bold prot-modeline-indicator-button)))
  "Face for modeline indicators with a background.
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-yellow
  '((default :inherit bold))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-yellow-bg
  '((default :inherit (bold prot-modeline-indicator-button)))
  "Face for modeline indicators with a background.
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-blue
  '((default :inherit bold))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-blue-bg
  '((default :inherit (bold prot-modeline-indicator-button)))
  "Face for modeline indicators with a background.
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-magenta
  '((default :inherit bold))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-magenta-bg
  '((default :inherit (bold prot-modeline-indicator-button)))
  "Face for modeline indicators with a background.
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-cyan
  '((default :inherit bold))
  "Face for modeline indicators (e.g. see my `notmuch-indicator').
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-cyan-bg
  '((default :inherit (bold prot-modeline-indicator-button)))
  "Face for modeline indicators with a background.
Colors are populated by `bard-modeline-setup-faces'."
  :group 'prot-modeline-faces)

;;;; Theme-aware face setup

(defvar bard-modeline--fallback-colors
  '((red      . "#880000") (red-bg      . "#aa1111") (red-fg-bg      . "white")
    (green    . "#005f00") (green-bg    . "#207b20") (green-fg-bg    . "white")
    (yellow   . "#6f4000") (yellow-bg   . "#805000") (yellow-fg-bg    . "white")
    (blue     . "#00228a") (blue-bg     . "#0000aa") (blue-fg-bg     . "white")
    (magenta . "#6a1aaf") (magenta-bg . "#6f0f9f") (magenta-fg-bg . "white")
    (cyan     . "#004060") (cyan-bg     . "#006080") (cyan-fg-bg     . "white"))
  "Fallback colors used when neither `modus-themes' nor `doric-themes' is active.")

(defun bard-modeline--set (face &optional fg bg)
  "Set FACE's foreground to FG and background to BG.
Explicitly clears attributes back to `unspecified' if they are nil,
ensuring old colors don't stick around during theme switches."
  (let ((fg-val (if (and fg (not (eq fg 'unspecified))) fg 'unspecified))
        (bg-val (if (and bg (not (eq bg 'unspecified))) bg 'unspecified)))
    (set-face-attribute face nil :foreground fg-val :background bg-val)))

(defun bard-modeline--setup-faces-modus ()
  "Populate indicator faces from the active `modus-themes' palette."
  (unless (fboundp 'modus-themes-get-color-value)
    (error "modus-themes-get-color-value is not available"))
  (cl-flet ((c (name) (modus-themes-get-color-value name)))
    (bard-modeline--set 'prot-modeline-indicator-red (c 'red) nil)
    (bard-modeline--set 'prot-modeline-indicator-red-bg (c 'fg-red-intense) (c 'bg-red-intense))
    (bard-modeline--set 'prot-modeline-indicator-green (c 'green) nil)
    (bard-modeline--set 'prot-modeline-indicator-green-bg (c 'fg-green-intense) (c 'bg-green-intense))
    (bard-modeline--set 'prot-modeline-indicator-yellow (c 'yellow) nil)
    (bard-modeline--set 'prot-modeline-indicator-yellow-bg (c 'fg-yellow-intense) (c 'bg-yellow-intense))
    (bard-modeline--set 'prot-modeline-indicator-blue (c 'blue) nil)
    (bard-modeline--set 'prot-modeline-indicator-blue-bg (c 'fg-blue-intense) (c 'bg-blue-intense))
    (bard-modeline--set 'prot-modeline-indicator-magenta (c 'magenta) nil)
    (bard-modeline--set 'prot-modeline-indicator-magenta-bg (c 'fg-magenta-intense) (c 'bg-magenta-intense))
    (bard-modeline--set 'prot-modeline-indicator-cyan (c 'cyan) nil)
    (bard-modeline--set 'prot-modeline-indicator-cyan-bg (c 'fg-cyan-intense) (c 'bg-cyan-intense))))

(defun bard-modeline--setup-faces-doric ()
  "Populate indicator faces from the active `doric-themes` palette.
Uses the fixed `bard-modeline--set' to prevent color leakage."
  (unless (fboundp 'doric-themes-with-colors)
    (error "doric-themes-with-colors is not available"))
  (doric-themes-with-colors
    (bard-modeline--set 'prot-modeline-indicator-red     fg-red     nil)
    (bard-modeline--set 'prot-modeline-indicator-green   fg-green   nil)
    (bard-modeline--set 'prot-modeline-indicator-yellow  fg-yellow  nil)
    (bard-modeline--set 'prot-modeline-indicator-blue    fg-blue    nil)
    (bard-modeline--set 'prot-modeline-indicator-magenta fg-magenta nil)
    (bard-modeline--set 'prot-modeline-indicator-cyan    fg-cyan    nil)

    (bard-modeline--set 'prot-modeline-indicator-red-bg     fg-main bg-red)
    (bard-modeline--set 'prot-modeline-indicator-green-bg   fg-main bg-green)
    (bard-modeline--set 'prot-modeline-indicator-yellow-bg  fg-main bg-yellow)
    (bard-modeline--set 'prot-modeline-indicator-blue-bg    fg-main bg-blue)
    (bard-modeline--set 'prot-modeline-indicator-magenta-bg fg-main bg-magenta)
    (bard-modeline--set 'prot-modeline-indicator-cyan-bg    fg-main bg-cyan)))

(doric-themes-with-colors
  (message "Foreground blue %s" fg-blue))

(defun bard-modeline--setup-faces-fallback ()
  "Populate indicator faces from `bard-modeline--fallback-colors'."
  (cl-flet ((c (key) (alist-get key bard-modeline--fallback-colors)))
    (bard-modeline--set 'prot-modeline-indicator-red (c 'red) nil)
    (bard-modeline--set 'prot-modeline-indicator-red-bg (c 'red-fg-bg) (c 'red-bg))
    (bard-modeline--set 'prot-modeline-indicator-green (c 'green) nil)
    (bard-modeline--set 'prot-modeline-indicator-green-bg (c 'green-fg-bg) (c 'green-bg))
    (bard-modeline--set 'prot-modeline-indicator-yellow (c 'yellow) nil)
    (bard-modeline--set 'prot-modeline-indicator-yellow-bg (c 'yellow-fg-bg) (c 'yellow-bg))
    (bard-modeline--set 'prot-modeline-indicator-blue (c 'blue) nil)
    (bard-modeline--set 'prot-modeline-indicator-blue-bg (c 'blue-fg-bg) (c 'blue-bg))
    (bard-modeline--set 'prot-modeline-indicator-magenta (c 'magenta) nil)
    (bard-modeline--set 'prot-modeline-indicator-magenta-bg (c 'magenta-fg-bg) (c 'magenta-bg))
    (bard-modeline--set 'prot-modeline-indicator-cyan (c 'cyan) nil)
    (bard-modeline--set 'prot-modeline-indicator-cyan-bg (c 'cyan-fg-bg) (c 'cyan-bg))))

;;;###autoload
(defun bard-modeline-setup-faces ()
  "Refresh `prot-modeline' indicator faces from the active theme flavor."
  (interactive)
  (cond
   ;; 1. Handle Modus, Ef, and Standard Themes Families (all use the modus framework)
   ((cl-some (lambda (theme)
               (let ((name (symbol-name theme)))
                 (or (string-prefix-p "modus-" name)
                     (string-prefix-p "ef-" name)
                     (string-prefix-p "standard-" name))))
             custom-enabled-themes)
    (when (fboundp 'modus-themes-with-colors)
      (modus-themes-with-colors
        (custom-set-faces
         `(prot-modeline-indicator-red ((,c :inherit bold :foreground ,red)))
         `(prot-modeline-indicator-green ((,c :inherit bold :foreground ,green)))
         `(prot-modeline-indicator-yellow ((,c :inherit bold :foreground ,yellow)))
         `(prot-modeline-indicator-blue ((,c :inherit bold :foreground ,blue)))
         `(prot-modeline-indicator-magenta ((,c :inherit bold :foreground ,magenta)))
         `(prot-modeline-indicator-cyan ((,c :inherit bold :foreground ,cyan)))
         `(prot-modeline-indicator-red-bg ((,c :inherit (bold prot-modeline-indicator-button) :background ,bg-red-intense :foreground ,fg-main)))
         `(prot-modeline-indicator-green-bg ((,c :inherit (bold prot-modeline-indicator-button) :background ,bg-green-intense :foreground ,fg-main)))
         `(prot-modeline-indicator-yellow-bg ((,c :inherit (bold prot-modeline-indicator-button) :background ,bg-yellow-intense :foreground ,fg-main)))
         `(prot-modeline-indicator-blue-bg ((,c :inherit (bold prot-modeline-indicator-button) :background ,bg-blue-intense :foreground ,fg-main)))
         `(prot-modeline-indicator-magenta-bg ((,c :inherit (bold prot-modeline-indicator-button) :background ,bg-magenta-intense :foreground ,fg-main)))
         `(prot-modeline-indicator-cyan-bg ((,c :inherit (bold prot-modeline-indicator-button) :background ,bg-cyan-intense :foreground ,fg-main)))))))

   ;; 2. Handle Doric Themes Flavor
   ((cl-some (lambda (theme) (string-prefix-p "doric-" (symbol-name theme)))
             custom-enabled-themes)
    (when (fboundp 'doric-themes-with-colors)
      (doric-themes-with-colors
        (custom-set-faces
         `(prot-modeline-indicator-red ((t :inherit bold :foreground ,fg-red)))
         `(prot-modeline-indicator-green ((t :inherit bold :foreground ,fg-green)))
         `(prot-modeline-indicator-yellow ((t :inherit bold :foreground ,fg-yellow)))
         `(prot-modeline-indicator-blue ((t :inherit bold :foreground ,fg-blue)))
         `(prot-modeline-indicator-magenta ((t :inherit bold :foreground ,fg-magenta)))
         `(prot-modeline-indicator-cyan ((t :inherit bold :foreground ,fg-cyan)))
         `(prot-modeline-indicator-red-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,bg-red :foreground ,fg-main)))
         `(prot-modeline-indicator-green-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,bg-green :foreground ,fg-main)))
         `(prot-modeline-indicator-yellow-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,bg-yellow :foreground ,fg-main)))
         `(prot-modeline-indicator-blue-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,bg-blue :foreground ,fg-main)))
         `(prot-modeline-indicator-magenta-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,bg-magenta :foreground ,fg-main)))
         `(prot-modeline-indicator-cyan-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,bg-cyan :foreground ,fg-main)))))))

   ;; 3. Fallback
   (t
    (cl-flet ((c (key) (alist-get key bard-modeline--fallback-colors)))
      (custom-set-faces
       `(prot-modeline-indicator-red ((t :inherit bold :foreground ,(c 'red))))
       `(prot-modeline-indicator-green ((t :inherit bold :foreground ,(c 'green))))
       `(prot-modeline-indicator-yellow ((t :inherit bold :foreground ,(c 'yellow))))
       `(prot-modeline-indicator-blue ((t :inherit bold :foreground ,(c 'blue))))
       `(prot-modeline-indicator-magenta ((t :inherit bold :foreground ,(c 'magenta))))
       `(prot-modeline-indicator-cyan ((t :inherit bold :foreground ,(c 'cyan))))
       `(prot-modeline-indicator-red-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,(c 'red-bg) :foreground ,(c 'red-fg-bg))))
       `(prot-modeline-indicator-green-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,(c 'green-bg) :foreground ,(c 'green-fg-bg))))
       `(prot-modeline-indicator-yellow-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,(c 'yellow-bg) :foreground ,(c 'yellow-fg-bg))))
       `(prot-modeline-indicator-blue-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,(c 'blue-bg) :foreground ,(c 'blue-fg-bg))))
       `(prot-modeline-indicator-magenta-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,(c 'magenta-bg) :foreground ,(c 'magenta-fg-bg))))
       `(prot-modeline-indicator-cyan-bg ((t :inherit (bold prot-modeline-indicator-button) :background ,(c 'cyan-bg) :foreground ,(c 'cyan-fg-bg)))))))))

(with-eval-after-load 'modus-themes
  (add-hook 'modus-themes-after-load-theme-hook #'bard-modeline-setup-faces))

(with-eval-after-load 'doric-themes
  (add-hook 'doric-themes-after-load-theme-hook #'bard-modeline-setup-faces))

(with-eval-after-load 'bard-theme
  (add-hook 'bard/after-theme-load-hook #'bard-modeline-setup-faces))

(bard-modeline-setup-faces)

;;;; Common helper functions

(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (prot-common-window-small-p)
       (> (length str) prot-modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun prot-modeline--truncate-p ()
  "Return non-nil if truncation should happen."
  (and (prot-common-window-small-p)
       (not (one-window-p :no-minibuffer))))

(defun prot-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR."
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
    str))

(defun prot-modeline-string-truncate-end (str)
  "Like `prot-modeline-string-truncate' but truncate from STR beginning."
  (if (prot-modeline--string-truncate-p str)
      (concat "..." (substring str (- prot-modeline-string-truncate-length)))
    str))

(defun prot-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun prot-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words."
  (if (prot-modeline--string-truncate-p str)
      (mapconcat #'prot-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun prot-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact."
  (if (prot-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'prot-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local prot-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'prot-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.")

;;;; Narrow indicator

(defvar-local prot-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'prot-modeline-indicator-cyan-bg)))
  "Mode line construct to report the narrowed state.")

;;;; Centered cursor indicator
(defvar-local bard-modeline-centered-cursor
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (bard/cursor-centered-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Center " 'face 'prot-modeline-indicator-yellow-bg)))
  "Mode line construct to report centered cursor.")

(defvar-local bard-modeline-ryo-modal-normal
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (not (bard/ryo-insert-p))
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize "<N>" 'face 'prot-modeline-indicator-magenta-bg)))
  "Mode line construct to show normal mode for ryo-modal.")

(defvar-local bard-modeline-ryo-modal-insert
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (bard/ryo-insert-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize "<I>" 'face 'prot-modeline-indicator-blue-bg)))
  "Mode line construct to show insert mode for ryo-modal.")

;;;; Input method

(defvar-local prot-modeline-input-method
    '(:eval
      (when (and (mode-line-window-selected-p)
                 current-input-method-title)
        (propertize (format " %s " current-input-method-title)
                    'face 'prot-modeline-indicator-green-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status
(defvar-local prot-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'prot-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Dedicated window

(defvar-local prot-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'prot-modeline-indicator-magenta-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;; Buffer name and modified status

(defun prot-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `prot-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(error italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun prot-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary."
  (when-let ((name (buffer-name)))
    (prot-modeline-string-truncate name)))

(defun prot-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (prot-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun prot-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `prot-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local prot-modeline-buffer-identification
    '(:eval
      (propertize (prot-modeline-buffer-name)
                  'face (prot-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (prot-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.")

;;;; Major mode

(defun prot-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'term-mode) ">_")
                    ((derived-mode-p 'emms-playlist-mode) "♪")
                    (t "δ"))))
    (propertize indicator 'face 'shadow)))

(defun prot-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun prot-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `prot-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local prot-modeline-major-mode
    (list
     (propertize "%[" 'face 'prot-modeline-indicator-red)
     '(:eval
       (concat
        (prot-modeline-major-mode-indicator)
        " "
        (propertize
         (prot-modeline-string-abbreviate-but-last
          (prot-modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (prot-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'prot-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local prot-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun prot-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar prot-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun prot-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun prot-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (prot-modeline--vc-help-echo file)
               'local-map prot-modeline-vc-map)))

(defun prot-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary."
  (prot-modeline-string-truncate
   (prot-modeline--vc-text file branch face)))

(defvar prot-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun prot-modeline--vc-get-face (key)
  "Get face from KEY in `prot-modeline--vc-faces'."
   (alist-get key prot-modeline--vc-faces 'up-to-date))

(defun prot-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (prot-modeline--vc-get-face (vc-state file backend)))

(defvar-local prot-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  (branch (prot-modeline--vc-branch-name file backend))
                  (face (prot-modeline--vc-face file backend)))
        (prot-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

(defun prot-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar prot-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro prot-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "prot-modeline-flymake-%s" type)) ()
     (when-let ((count (prot-modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                     'mouse-face 'mode-line-highlight
                     'local-map prot-modeline-flymake-map
                     'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(prot-modeline-flymake-type error "☣")
(prot-modeline-flymake-type warning "!")
(prot-modeline-flymake-type note "·" success)

(defvar-local prot-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         '(:eval (prot-modeline-flymake-error))
         '(:eval (prot-modeline-flymake-warning))
         '(:eval (prot-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.")

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local prot-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.")

(defvar-local prot-modeline-notmuch-indicator
    '(notmuch-indicator-mode
      (" "
       (:eval (when (mode-line-window-selected-p)
                notmuch-indicator--counters))))
  "The equivalent of `notmuch-indicator-mode-line-construct'.")

(defvar-local bard-evil-state-indicator
  '(:eval
    (when (and (bound-and-true-p evil-local-mode)
               (mode-line-window-selected-p))
      (let ((state-label
             (pcase evil-state
               ('normal  (propertize " <N>" 'face 'prot-modeline-indicator-green))
               ('insert  (propertize " <I> " 'face 'prot-modeline-indicator-blue))
               ('visual  (propertize " <V> " 'face 'prot-modeline-indicator-yellow))
               ('replace (propertize " <R> " 'face 'prot-modeline-indicator-red))
               ('emacs   (propertize " <E> " 'face 'prot-modeline-indicator-magenta))
               ('motion  (propertize " <V> " 'face 'prot-modeline-indicator-cyan))
               (_        (propertize " <> " 'face 'shadow)))))
        state-label)))
  "Modeline indicator for current Evil state.")


;;;; Org clock indicator
(defun bard-modeline--org-clock-check-overrun ()
  "Check if the clock is overrun."
  (when org-clock-effort
    (> (org-clock-get-clocked-time)
       (org-duration-to-minutes org-clock-effort))))

(defun bard-modeline--org-clock-string ()
  "Make a string for org-clock indicator."
  (let* ((clocked-mins (org-clock-get-clocked-time))
         (work-time (org-duration-from-minutes clocked-mins)))
    (if org-clock-effort
        (format "[%s/%s]"
                work-time
                org-clock-effort)
      (format "[%s]" work-time))))

(defun bard-modeline--org-clock-choose-face ()
  "Make the background red when overrunning the clock."
  (if (bard-modeline--org-clock-check-overrun)
      'prot-modeline-indicator-red-bg
    'prot-modeline-indicator-green-bg))

(defvar-local bard-modeline-org-clock
  '(:eval
    (when (and (featurep 'org)
               (bound-and-true-p org-clock-current-task)
               (mode-line-window-selected-p))
      (propertize
       (concat " ⊙ "
               (prot-modeline-string-truncate
                (bard-modeline--org-clock-string))
               " ")
       'face (bard-modeline--org-clock-choose-face)
       'mouse-face 'mode-line-highlight
       'help-echo "Org clocked task\nmouse-1: Go to task"
       'local-map (let ((map (make-sparse-keymap)))
                    (define-key map [mode-line down-mouse-1]
                      #'org-clock-goto)
                    map))))
  "Mode line construct showing the current Org clocked task.")

;;;; Miscellaneous

(with-eval-after-load 'org
  (setq mode-line-misc-info
        (delete 'org-mode-line-string mode-line-misc-info)))

(defvar-local prot-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.")

(defvar-local prot-modeline-frame-name
    '(:eval
      (when-let* ((_ (mode-line-window-selected-p))
                  (current-frame (selected-frame))
                  (_ (frame-live-p current-frame))
                  (parameters (frame-parameters))
                  (name (capitalize (alist-get 'name parameters))))
        (format "%s %s "
                "@"
                (propertize name
                            'face 'prot-modeline-indicator-blue
                            'mouse-face 'mode-line-highlight
                            'help-echo (format "Current frame name\nmouse-1: `buffer-menu'")))))
  "Mode line construct to display the current frame name.")

;;;; Risky local variables

(dolist (construct '(prot-modeline-kbd-macro
                     prot-modeline-narrow
                     bard-modeline-centered-cursor
                     bard-evil-state-indicator
                     prot-modeline-input-method
                     bard-modeline-org-clock
                     prot-modeline-buffer-status
                     prot-modeline-window-dedicated-status
                     prot-modeline-frame-name
                     prot-modeline-evil
                     prot-modeline-buffer-identification
                     prot-modeline-major-mode
                     prot-modeline-process
                     prot-modeline-vc-branch
                     prot-modeline-flymake
                     prot-modeline-eglot
                     prot-modeline-misc-info
                     prot-modeline-notmuch-indicator))
  (put construct 'risky-local-variable t))

(provide 'bard-modeline)
;;; bard-modeline.el ends here
