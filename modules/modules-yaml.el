;;; modules-yaml.el -*- lexical-binding: t; -*-

;; Major mode for editing yaml files.
(use-package yaml-mode
  :mode
  (("\\.yml\\'" . yaml-mode)))

;; Yaml syntax checking.
(defvar flycheck-disabled-checkers)
(defun configure-yaml-syntax-checking ()
  (flycheck-mode))

(add-hook 'yaml-mode-hook #'configure-yaml-syntax-checking)

(provide 'modules-yaml)
;;; modules-yaml.el ends here
