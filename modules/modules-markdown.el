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

(provide 'modules-markdown)
;;; modules-markdown.el ends here
