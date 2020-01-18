;;; core-packages.el -*- lexical-binding: t; -*-

;; Move Custom-Set-Variables to seperate file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Configure package archives, where to install packages and add them to `load-path', skip when compiled.
(eval-when-compile
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
        package-archives
        '(("elpa"         . "https://elpa.gnu.org/packages/")
          ("elpa-mirror"  . "https://raw.githubusercontent.com/Cliffzz/.emacs.d/master/elpa-mirror/")
          ("melpa"        . "https://melpa.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/"))
        package-archive-priorities
        '(("elpa"         . 5)
          ("elpa-mirror"  . 15)
          ("melpa"        . 10)
          ("melpa-stable" . 0))))

;; Install `use-package' automatically if it isn't installed, assume it's installed if `package-user-dir' exists,
;; skip when compiled.
(eval-when-compile
  (unless (file-directory-p (symbol-value 'package-user-dir))
    (package-refresh-contents)
    (package-install 'use-package)))

;; Manually call `package-initalize' when running in `noninteractive' mode.
(eval-and-compile
  (if noninteractive
      (package-initialize)))

;; Load `use-package' and ensure packages ara always installed, skip when compiled.
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Dependency for `use-package' `binds:'.
(use-package bind-key)

;; Elpa mirror, create local Emacs package repository from installed packages.
(use-package elpa-mirror
  :defer t
  :commands
  (elpamr-create-mirror-for-installed))

;; Move Custom-Set-Variables to seperate file.
(setq custom-file (concat user-emacs-directory ".cache/custom.el"))
(load custom-file 'noerror)

;; Keep Emacs temporary files in `.cache' directory.
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name ".cache/etc" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name ".cache/var" user-emacs-directory)))

(provide 'core-packages)
;;; core-packages.el ends here
