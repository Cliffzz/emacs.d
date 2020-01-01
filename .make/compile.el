;;; compile.el -*- lexical-binding: t; -*- -*-no-byte-compile: t; -*-

;; Load `early-init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "early-init.el"))

;; Load `init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "init.el"))

;; Compile core.
(byte-recompile-directory (concat user-emacs-directory "core"))

;; Compile init files
(byte-compile-file (concat user-emacs-directory "early-init.el"))
(byte-compile-file (concat user-emacs-directory "init.el"))

(provide 'compile)

;; Local Variables:
;; no-byte-compile: t
;;; compile.el ends here
