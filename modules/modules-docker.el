;;; modules-docker.el -*- lexical-binding: t; -*-

;; Major mode for editing docker files.
(use-package dockerfile-mode
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

;; Enable langauge server completion.
(defun setup-docker-completion ()
  (defvar company-backends)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-lsp)))

(add-hook 'dockerfile-mode-hook #'setup-docker-completion)

;; Docker syntax checking with language server.
(defvar flycheck-disabled-checkers)
(defun configure-docker-syntax-checking ()
  (flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(dockerfile-hadolint)))

  (defvar lsp-ui-sideline-show-hover)
  (make-local-variable 'lsp-ui-sideline-show-hover)
  (setq lsp-ui-sideline-show-hover nil))

(add-hook 'dockerfile-mode-hook #'configure-docker-syntax-checking)

;; Enable langauge server.
(add-hook 'dockerfile-mode-hook #'lsp)

(provide 'modules-docker)
;;; modules-docker.el ends here
