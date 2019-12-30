;;; core.el -*- lexical-binding: t; -*-

;; Ensure `core-dir' is in `load-path'.
(add-to-list 'load-path (file-name-directory load-file-name))

;; Restore `file-name-handler-alist' to it's original value, because it is needed for handling encrypted or compressed
;; files.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist *file-name-handler-alist-original*)))

;; Bootstrap Emacs config.
(defun initalize ()
  (require 'core-packages))

;; Load Emacs core config files for an interactive session.
(defun initialize-core ()
  (require 'core-ui)
  (require 'core-editor))

(provide 'core)
;;; core.el ends here
