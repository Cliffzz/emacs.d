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

;; Prettier, code formatting.
(use-package prettier-js
  :commands (prettier-js))

(defun set-prettier-ts-config ()
  (interactive)
  (make-local-variable 'prettier-js-command)
  (make-local-variable 'prettier-js-args)
  (setq prettier-js-command "prettier-eslint")
  (setq prettier-js-args '("--print-width" "100" "--tab-width" "4" "--single-quote" "--trailing-comma" "all"))
  (bind-key "C-c f" 'prettier-js typescript-mode-map))

(defun set-prettier-tsx-config ()
  (make-local-variable 'prettier-js-command)
  (make-local-variable 'prettier-js-args)
  (setq prettier-js-command "prettier-eslint")
  (setq prettier-js-args '("--print-width" "100" "--tab-width" "4" "--single-quote" "--trailing-comma" "all"))
  (bind-key "C-c f" 'prettier-js web-mode-map))

(add-hook 'typescript-mode-hook #'set-prettier-ts-config)
(set-tsx-mode-hook #'set-prettier-tsx-config)

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
