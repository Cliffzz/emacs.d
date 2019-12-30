;;; install.el -*- lexical-binding: t; -*- -*-no-byte-compile: t; -*-

;; Load `early-init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "early-init.el"))

;; Load `init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "init.el"))

;; Compile core.
(byte-recompile-directory (concat user-emacs-directory "core") 0)

(provide 'install)
;;; install.el ends here
