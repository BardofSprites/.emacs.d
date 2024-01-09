(setq auth-sources '("~/.authinfo.gpg"))

(defun my-fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun my-nickserv-password (server)
  (my-fetch-password :user "bardman" :machine "irc.libera.chat"))

(setq circe-network-options
      '(("Libera Chat"
         :nick "bardman"
         :channels ("#emacs" "##anime" "#gentoo")
         :nickserv-password my-nickserv-password)))


(provide 'bard-emacs-circe)
