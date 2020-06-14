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

;; Enable langauge server.
(set-html-mode-hook #'lsp)

(provide 'modules-html)
;;; modules-html.el ends here
