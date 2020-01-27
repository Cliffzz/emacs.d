;;; core-path.el -*- lexical-binding: t; -*-

;; Setup path.
(use-package exec-path-from-shell
  :if (< (length command-line-args) 2)
  :config
  (exec-path-from-shell-initialize))

(provide 'core-path)
;;; core-path.el ends here
