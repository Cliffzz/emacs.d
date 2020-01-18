;;; modules-html.el -*- lexical-binding: t; -*-

;; Only run `web-mode-hook', when file is a `html' file.
(defun set-html-mode-hook (hook-to-run)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "html" (file-name-extension buffer-file-name))
                (funcall hook-to-run)))))

;; Major mode for editing html files.
(use-package web-mode
  :mode
  ("\\.html\\'" . web-mode)
  :config
  (defvar web-mode-markup-indent-offset)
  (setq web-mode-markup-indent-offset 4))

;; Prettier, code formatting.
(use-package prettier-js
  :commands (prettier-js)
  :init
  (declare-function set-prettier-html-config "modules-html")
  (defun set-prettier-html-config ()
    (make-local-variable 'prettier-js-args)
    (setq prettier-js-args '("--print-width" "100" "--tab-width" "4" "--single-quote" "--trailing-comma" "all"))
    (bind-key "C-c f" 'prettier-js web-mode-map))

  (set-html-mode-hook #'set-prettier-html-config))

;; Enable langauge server completion.
(defun setup-html-completion ()
  (defvar company-backends)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-lsp)))

(set-html-mode-hook #'setup-html-completion)

;; HTML syntax checking with language server.
(defvar flycheck-disabled-checkers)
(defun configure-html-syntax-checking ()
  (flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(html-tidy)))

  (defvar lsp-ui-sideline-show-hover)
  (make-local-variable 'lsp-ui-sideline-show-hover)
  (setq lsp-ui-sideline-show-hover nil))

(set-html-mode-hook #'configure-html-syntax-checking)

;; Enable langauge server.
(set-html-mode-hook #'lsp)

(provide 'modules-html)
;;; modules-html.el ends here
