;;; modules-docker.el -*- lexical-binding: t; -*-

;; Major mode for editing docker files.
(use-package dockerfile-mode
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

;; Enable langauge server.
(add-hook 'dockerfile-mode-hook #'lsp)

(provide 'modules-docker)
;;; modules-docker.el ends here
