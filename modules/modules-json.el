;;; modules-json.el -*- lexical-binding: t; -*-

;; Major mode for editing JSON files.
(use-package json-mode
  :mode
  (("\\.json?\\'" . json-mode))
  :config
  (defvar js-indent-level)
  (defvar json-reformat:indent-width)
  (defun json-mode-indentation ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2
          json-reformat:indent-width 2))
  (add-hook 'json-mode-hook 'json-mode-indentation))

;; JSON syntax checking with jsonlint.
(defvar flycheck-disabled-checkers)
(defun configure-json-syntax-checking ()
  (flycheck-mode)
  ;; Flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-python-json))))

(add-hook 'json-mode-hook #'configure-json-syntax-checking)

(provide 'modules-json)
;;; modules-json.el ends here
