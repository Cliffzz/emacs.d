;;; modules-typescript.el -*- lexical-binding: t; -*-

;; Only run `web-mode-hook', when file is a `tsx' file.
(defun set-tsx-mode-hook (hook-to-run)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (funcall hook-to-run)))))

;; Major mode for editing typescript files.
(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode))

;; Major mode for editing tsx files.
(use-package web-mode
  :mode
  ("\\.tsx\\'" . web-mode))

;; Add project node modules to path.
(use-package add-node-modules-path
  :commands
  (add-node-modules-path)
  :init
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (set-tsx-mode-hook #'add-node-modules-path))

;; Enable langauge server completion.
(defun setup-ts-completion ()
  (defvar company-backends)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-lsp)))

(add-hook 'typescript-mode-hook #'setup-ts-completion)
(set-tsx-mode-hook #'setup-ts-completion)

;; Typescript syntax checking with eslint + language server.
(defvar flycheck-disabled-checkers)
(defun configure-ts-syntax-checking ()
  (flycheck-mode)

  (declare-function flycheck-add-mode "flycheck")

  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(typescript-tslint)))

  (declare-function flycheck-add-next-checker "flycheck")
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint))

(add-hook 'typescript-mode-hook #'configure-ts-syntax-checking)
(set-tsx-mode-hook #'configure-ts-syntax-checking)

;; Enable langauge server.
(add-hook 'typescript-mode-hook #'lsp)
(set-tsx-mode-hook #'lsp)

(provide 'modules-typescript)
;;; modules-typescript.el ends here
