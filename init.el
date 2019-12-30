;;; init.el -*- lexical-binding: t; -*-

;; Load the core.
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; And set it loose.
(initalize)
(unless noninteractive
    (initialize-core))

(provide 'init)
;;; init.el ends here
