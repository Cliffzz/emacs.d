;;; modules-pug.el -*- lexical-binding: t; -*-

;; ;; Major mode for editing jade/pug files.
(use-package pug-mode
  :mode
  (("\\.jade\\'" . pug-mode))
  ("\\.pug\\'" . pug-mode))

(provide 'modules-pug)
;;; modules-pug.el ends here
