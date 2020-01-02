;;; init.el -*- lexical-binding: t; -*-

;; Load the core.
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; Load the modules.
(load (concat user-emacs-directory "modules/modules")
      nil 'nomessage)

;; And set it loose.
(initalize)
(unless noninteractive
  (progn
    (initialize-core)
    (initialize-modules)))

(provide 'init)
;;; init.el ends here
