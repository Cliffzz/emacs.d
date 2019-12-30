;;; core-editor.el -*- lexical-binding: t; -*-

;; Force spaces for indentation.
(setq-default indent-tabs-mode nil)

;; Mitigate performance problems with long lines.
(global-so-long-mode)

;; Add a newline automatically at the end of the file.
(setq require-final-newline t)

;; Disable backup files.
(setq make-backup-files nil)

;; Disable file locks.
(setq create-lockfiles nil)

;; Disable auto sae files.
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;; Set remote path as `TRAMP' path, after `TRAMP' has been loaded.
(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Unobtrusively trim spaces from end of line.
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; Smartly manage pairs.
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

;; Universal go to definition.
(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-." . dumb-jump-go)
         ("C-," . dumb-jump-back)))
  :config
  (setq dumb-jump-selector 'ivy))

;; Easy expandable selectable region.
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;; Jump to character in visiable text.
(use-package avy
  :bind
  (("C-'" . 'avy-goto-char-timer)))

;; Provide a visualization of undos in a file.
(use-package undo-tree
  :commands
  (global-undo-tree-mode)
  :init
  (global-undo-tree-mode)
  :bind (("C-/" . 'undo-tree-undo)
         ("C-?" . 'undo-tree-redo)))

;; Navigate windows and frames using numbers.
(use-package winum
  :config
  (winum-mode))

(provide 'core-editor)
;;; core-editor.el ends here
