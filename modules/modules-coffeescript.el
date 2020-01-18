;;; modules-coffeescript.el -*- lexical-binding: t; -*-

;; Major mode for editing coffeescript files.
(use-package coffee-mode
  :mode
  (("\\.coffee\\'" . coffee-mode))
  :config
  (defvar coffee-tab-width)
  (setq coffee-tab-width 2))

;; Add project node modules to path.
(use-package add-node-modules-path
  :commands
  (add-node-modules-path)
  :init
  (add-hook 'coffee-mode-hook #'add-node-modules-path))

;; Coffeescript syntax checking with coffeelint.
(defun configure-coffeescript-syntax-checking ()
  (flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(coffee))))

(add-hook 'coffee-mode-hook #'configure-coffeescript-syntax-checking)

(provide 'modules-coffeescript)
;;; modules-coffeescript.el ends here
