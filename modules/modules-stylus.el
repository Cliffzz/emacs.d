;;; modules-stylus.el -*- lexical-binding: t; -*-

;; Major mode for editing stylus files.
(use-package stylus-mode
  :load-path "site-elisp/stylus-mode"
  :mode
  (("\\.styl\\'" . stylus-mode)))

(provide 'modules-stylus)
;;; modules-stylus.el ends here
