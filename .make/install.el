;;; install.el -*- lexical-binding: t; -*- -*-no-byte-compile: t; -*-

;; Load `early-init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "early-init.el"))

;; Load `init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "init.el"))

;; Load all core modules to trigger package installation.
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "core/*.el")))

;; Load all modules to trigger package installation.
(mapc 'load (file-expand-wildcards (concat user-emacs-directory "modules/*.el")))

(provide 'install)

;;; install.el ends here
