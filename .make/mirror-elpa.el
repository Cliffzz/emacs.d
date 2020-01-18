;;; mirror-elpa.el -*- lexical-binding: t; -*- -*-no-byte-compile: t; -*-

;; Load `early-init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "early-init.el"))

;; Load `init', this isn't automatically loaded in `noninteractive' mode.
(load (concat user-emacs-directory "init.el"))

(elpamr-create-mirror-for-installed (concat user-emacs-directory "elpa-mirror") t)

;; Local Variables:
;; no-byte-compile: t
;;; mirror-elpa.el ends here
