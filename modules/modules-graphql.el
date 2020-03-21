;;; modules-graphql.el -*- lexical-binding: t; -*-

;; Major mode for editing graphql files.
(use-package graphql-mode
  :mode
  (("\\.graphql\\'" . graphql-mode))
  :config
  (setq graphql-indent-level 4))

;; Prettier, code formatting.
(use-package prettier-js
  :commands (prettier-js))

(defun set-prettier-graphql-config ()
  (make-local-variable 'prettier-js-args)
  (setq prettier-js-args '("--print-width" "100" "--tab-width" "4" "--single-quote" "--trailing-comma" "all"))
  (bind-key "C-c f" 'prettier-js graphql-mode-map))

(add-hook 'graphql-mode-hook #'set-prettier-graphql-config)

(provide 'modules-graphql)
;;; modules-graphql.el ends here
