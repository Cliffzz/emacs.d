;;; modules-graphql.el -*- lexical-binding: t; -*-

;; Major mode for editing graphql files.
(use-package graphql-mode
  :mode
  (("\\.graphql\\'" . graphql-mode))
  :config
  (setq graphql-indent-level 4))

(provide 'modules-graphql)
;;; modules-graphql.el ends here
