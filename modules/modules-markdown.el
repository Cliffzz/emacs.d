;;; modules-markdown.el -*- lexical-binding: t; -*-

;; Major mode for editing markdown files.
(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config
  (defvar markdown-command)
  (setq markdown-command "multimarkdown"))

;; Markdown table of contents
(use-package markdown-toc
  :commands (markdown-toc-generate-or-refresh-toc))

;; Prettier, code formatting.
(use-package prettier-js
  :commands (prettier-js)
  :init
  (declare-function set-prettier-markdown-config "modules-markdown")
  (defun set-prettier-markdown-config ()
    (make-local-variable 'prettier-js-args)
    (setq prettier-js-args '("--print-width" "100" "--tab-width" "4" "--single-quote" "--trailing-comma" "all"))
    (bind-key "C-c f" 'prettier-js markdown-mode-map))

  (add-hook 'markdown-mode-hook #'set-prettier-markdown-config))

(provide 'modules-markdown)
;;; modules-markdown.el ends here
