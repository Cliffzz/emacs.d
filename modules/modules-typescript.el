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

;; Enable langauge server.
(add-hook 'typescript-mode-hook #'lsp)
(set-tsx-mode-hook #'lsp)

(provide 'modules-typescript)
;;; modules-typescript.el ends here
