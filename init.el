;;; init.el --- Emacs init. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Cliffz's Emacs configuration
;;
;;; Code:

;; Increase init performance by increasing garbage collect threshold.
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defun cliffz-reset-gc-cons ()
  "Reset gc-cons to default."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'cliffz-reset-gc-cons)

;; Increase init performance by temporary unsetting file-name-handler.
(defvar cliffz-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun cliffz-reset-file-name-handler-alist ()
  "Reset 'file-name-handler-alist' to default."
  (setq file-name-handler-alist cliffz-file-name-handler-alist))
(add-hook 'emacs-startup-hook 'cliffz-reset-file-name-handler-alist)

;; Setup external file for custom settings
(setq custom-file "~/.emacs.d/custom-settings.el")

;; Start maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable toolbars.
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Disable scrollbars on new frame.
(defun cliffz-base-defaults-disable-scroll-bars (frame)
  "Disable FRAME scroll bars."
  (menu-bar-mode -1)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'cliffz-base-defaults-disable-scroll-bars)

;; Disable alarm.
(setq ring-bell-function 'ignore)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Disable startup message.
(setq inhibit-startup-message t)

;; Disable default welcome message.
(defun display-startup-echo-area-message ()
  "Disable default welcome message."
  (message nil))

;; Disable scratch message.
(setq initial-scratch-message nil)

;; Start in text-mode
(setq initial-major-mode 'text-mode)

;; Always blink cursor.
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

;; Change yes/no to y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Prefer utf8.
(prefer-coding-system 'utf-8)

;; Indent using spaces.
(setq-default indent-tabs-mode nil)

;; Network security, disabled for CI.
(unless (string= (getenv "CI") "true")
  (defvar network-security-level)
  (defvar nsm-settings-file)
  (setq network-security-level 'high)
  (setq nsm-settings-file "~/.emacs.d/.cache/network-security.data"))

;; Smooth scrolling.
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

;; Horizontal scrolling.
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Mode line.
(setq line-number-mode t
      column-number-mode t)

;; Font setup.
(defvar cliffz-font-size 130)
(cond ((eq system-type 'windows-nt)
       (setq cliffz-font-size 100)))

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height cliffz-font-size
                    :weight 'normal
                    :width 'normal)

;; Highlight current line.
(global-hl-line-mode 1)

;; Truncate lines.
(setq-default truncate-lines t)

;; Highlight parentheses.
(show-paren-mode t)

;; Line numbers.
(defvar display-line-numbers-grow-only)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-grow-only t)

;; Disable eldoc mode.
(global-eldoc-mode -1)

;; TRAMP
(defvar tramp-persistency-file-name)
(setq tramp-persistency-file-name "~/.emacs.d/.cache/tramp")
(cond ((eq system-type 'windows-nt)
       (defvar tramp-default-method)
       (setq tramp-default-method "plink")))

;; Recent files file location.
(defvar recentf-save-file)
(setq recentf-save-file "~/.emacs.d/.cache/recentf")

;; Backups.
(defvar backup-dir (expand-file-name "~/.emacs.d/.cache/backups/"))
(defvar tramp-backup-directory-alist)
(setq backup-directory-alist ( list (cons ".*" backup-dir))
      tramp-backup-directory-alist backup-directory-alist
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 12
      kept-old-versions 4)

;; Auto saves.
(defvar autosave-dir (expand-file-name "~/.emacs.d/.cache/autosaves/"))
(defvar tramp-auto-save-directory)
(setq auto-save-list-file-prefix autosave-dir
      auto-save-file-name-transforms `((".*" ,autosave-dir t))
      tramp-auto-save-directory autosave-dir)

;; Disable package.el initialization by default.
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Make sure elpa directory exists.
(eval-when-compile
  (unless (file-directory-p "~/.emacs.d/elpa")
    (make-directory "~/.emacs.d/elpa" t)))

;; Initialize load-path for packages.
(setq load-path (eval-when-compile (append load-path (directory-files "~/.emacs.d/elpa" t "^[^.]" t))))
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/gruvbox-theme-20180313.1451")

;; Initialize package.el only at compile time.
(eval-when-compile
  (require 'package)
  (setq package-archives
        '(("ELPA Mirror"  . "https://raw.githubusercontent.com/Cliffzz/.emacs.d/master/elpa-mirror/")
          ("MELPA"        . "https://melpa.org/packages/")
          ("MELPA Stable" . "https://stable.melpa.org/packages/")
          ("GNU ELPA"     . "http://elpa.gnu.org/packages/"))
        package-archive-priorities
        '(("ELPA Mirror"  . 3)
          ("MELPA"        . 2)
          ("MELPA Stable" . 1)
          ("GNU ELPA"     . 0)))
  (package-initialize)

  ;; Install and loadd use-package.
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

;; Ensure packages are installed automatically.
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Change mode line names.
(use-package delight
  :demand t)

;; Dependency for custom keybinds.
(use-package bind-key
  :demand t)

;; Elpa mirror.
(use-package elpa-mirror
  :commands (elpamr-create-mirror-for-installed))

;; Set theme.
(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-medium t))

;; Bind compile files command.
(use-package compile-files
  :load-path "lisp/compile-files"
  :commands (compile-files)
  :bind (("C-c c" . 'compile-files)))

;; Fix path variables in macOS.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :commands (exec-path-from-shell-initialize)
  :config
  (setq exec-path
        (or (eval-when-compile
              (when (require 'exec-path-from-shell nil t)
                (setq exec-path-from-shell-check-startup-files nil
                      exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
                (exec-path-from-shell-initialize)
                exec-path))
            exec-path)))

;; Delete trailing whitespaces on save.
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :init
  (defun cliffz-show-trailing-whitespace ()
    "Show trailing whitespace."
    (setq show-trailing-whitespace 1))
  (add-hook 'prog-mode-hook 'cliffz-show-trailing-whitespace))

;; Spell checking.
(use-package flyspell
  :delight flyspell-mode
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))
  ;; Fix word correction suggestions.
  (defadvice flyspell-correct-word (around my-flyspell-correct-word activate)
    ;; Kill ispell and reset arguments.
    (ispell-kill-ispell t)
    (setq ispell-extra-args '(""))
    ad-do-it
    ;; Restore camel case arguments.
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))
    (ispell-kill-ispell t)))

;; Syntax checking.
(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :commands (flycheck-add-mode)
  :config
  (cond ((eq system-type 'windows-nt)
         (setq flycheck-yaml-jsyaml-executable "~/.emacs.d/node_modules/.bin/js-yaml.cmd")))
  (cond ((eq system-type 'darwin)
         (setq flycheck-yaml-jsyaml-executable "~/.emacs.d/node_modules/.bin/js-yaml")))
  ;; Disabled checkers.
  (add-to-list 'flycheck-disabled-checkers 'json-python-json)
  ;; Eanble tslint for tsx files.
  (defun cliffz-enable-tslint-tsx ()
    "Enable tslint for tsx files."
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (flycheck-add-mode 'typescript-tslint 'web-mode)))
  (add-hook 'web-mode-hook 'cliffz-enable-tslint-tsx)
  ;; Flycheck Theme.
  (declare-function flycheck-define-error-level "flycheck")
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (let ((bitmap 'my-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-info))

  (set-face-attribute 'flycheck-fringe-error nil :foreground "#fb4933")
  (set-face-attribute 'flycheck-fringe-warning nil :foreground "#fabd2f")
  (set-face-attribute 'flycheck-fringe-info nil :foreground "#83a598")

  (set-face-attribute 'flycheck-error nil :underline '(:style line :color "#fb4933"))
  (set-face-attribute 'flycheck-warning nil :underline '(:style line :color "#fabd2f"))
  (set-face-attribute 'flycheck-info nil :underline '(:style line :color "#83a598")))

;; Keybinds auto completion.
(use-package which-key
  :delight which-key-mode
  :commands (which-key-mode)
  :init
  (which-key-mode))

;; Code auto completion.
(use-package company
  :delight company-mode
  :hook (prog-mode . global-company-mode))

;; Smartly manage pairs.
(use-package smartparens
  :delight smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; Color parentheses.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Project integration.
(use-package projectile
  :delight projectile-mode
  :hook (prog-mode . projectile-mode)
  :config
  (defvar projectile-sort-order)
  (defvar projectile-cache-file)
  (defvar projectile-known-projects-file)
  (setq projectile-sort-order 'recentf
        projectile-cache-file "~/.emacs.d/.cache/projectile.cache"
        projectile-known-projects-file "~/.emacs.d/.cache/projectile-bookmarks.eld"))

;; Mode line setup.
(use-package smart-mode-line
  :commands (sml/setup)
  :init
  (setq line-number-mode 1
        column-number-mode t
        sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 40
        sml/mode-width 'full)
  (sml/setup))

;; Emacs completion using ivy.
(use-package ivy
  :delight ivy-mode
  :commands (ivy-mode ivy-resume)
  :bind (("C-c C-r" . 'ivy-resume)
         ("<f6>" . 'ivy-resume))
  :init
  (ivy-mode)
  :config
  (defvar ivy-use-virtual-buffers)
  (defvar ivy-height 20)
  (defvar ivy-count-format)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-height 20
        ivy-count-format "(%d/%d) "))

;; Emacs commands completion using ivy.
(use-package counsel
  :after (ivy)
  :commands
  (counsel-M-x
   counsel-find-file
   counsel-describe-function
   counsel-describe-variable
   counsel-info-lookup-symbol
   counsel-unicode-char
   counsel-rg)
  :bind (("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("<f1> f" . 'counsel-describe-function)
         ("<f1> v" . 'counsel-describe-variable)
         ("<f2> i" . 'counsel-info-lookup-symbol)
         ("<f2> u" . 'counsel-unicode-char)
         ("C-c s" . 'counsel-rg)))

;; Search replacement using ivy.
(use-package swiper
  :after (ivy)
  :commands (swiper)
  :bind (("\C-s" . 'swiper)))

;; Search with ripgrep.
(use-package ripgrep
  :commands (ripgrep-regexp)
  :bind (("C-c r" . 'ripgrep-regexp)))

(use-package eshell
  :commands (eshell)
  :config
  ;; Don't pause the output through $PAGER
  (setenv "PAGER" "cat")

  (defvar eshell-cmpl-cycle-completions)
  (defvar eshell-history-size)
  (defvar eshell-hist-ignoredups)
  (defvar eshell-buffer-shorthand)
  (defvar eshell-highlight-prompt)
  (defvar eshell-plain-echo-behavior)
  (setq eshell-cmpl-cycle-completions nil
        eshell-buffer-maximum-lines 20000
        eshell-history-size 350
        eshell-hist-ignoredups t
        eshell-buffer-shorthand t
        eshell-highlight-prompt nil
        eshell-plain-echo-behavior t
        eshell-directory-name "~/.emacs.d/.cache/eshell/")

  ;; Visual commands
  (defvar eshell-visual-commands)
  (defun cliffz-set-eshell-visual-commands ()
    "Set eshell visual commands."
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "tail"))
  (add-hook 'eshell-mode-hook 'cliffz-set-eshell-visual-commands))

;; Eshell prompt settings.
(use-package eshell-prompt-extras
  :after (esh-opt)
  :demand t
  :config
  (defvar eshell-highlight-prompt)
  (defvar eshell-prompt-function)
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;; Eshell color output.
(use-package xterm-color
  :after (em-prompt)
  :demand t
  :config
  (defvar xterm-color-preserve-properties)
  (defun cliffz-set-xterm-color-properties ()
    "Set xterm color properties."
         (setq xterm-color-preserve-properties t))
  (add-hook 'eshell-before-prompt-hook 'cliffz-set-xterm-color-properties)
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

;; Git.
(use-package magit
  :delight auto-revert-mode
  :commands (magit-status)
  :bind (("C-x g" . 'magit-status)))

;; Highlight git changes.
(use-package diff-hl
  :delight diff-hl-mode
  :hook (prog-mode . diff-hl-mode)
  :config
  (vc-mode 1)
  (defvar diff-hl-side)
  (setq diff-hl-side 'right)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (set-face-attribute 'diff-hl-change nil :foreground "#83a598" :background "#282828")
  (set-face-attribute 'diff-hl-delete nil :foreground "#fb4933")
  (set-face-attribute 'diff-hl-insert nil :foreground "#b8bb26"))

;; Undo tree.
(use-package undo-tree
  :delight undo-tree-mode
  :commands (global-undo-tree-mode)
  :init
  (global-undo-tree-mode)
  (declare-function undo-tree-undo "undo-tree")
  (declare-function undo-tree-redo "undo-tree")
  :bind (("C-/" . 'undo-tree-undo)
         ("C-?" . 'undo-tree-redo)))

;; Go to definition.
(use-package dumb-jump
  :bind (("C-c ." . dumb-jump-go)
         ("C-c ," . dumb-jump-back)))

;; Expand region.
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Jump to character.
(use-package avy
  :commands (avy-goto-char)
  :bind (("C-'" . 'avy-goto-char)))

;; Jump to window.
(use-package ace-window
  :commands (ace-window)
  :bind (("C-x o" . 'ace-window)))

;; Snippets.
(use-package yasnippet
  :delight yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (defvar yas-snippet-dirs)
  (declare-function yas-reload-all "yasnippet")
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-reload-all))

;; JSON mode.
(use-package json-mode
  :mode
  (("\\.json?\\'" . json-mode))
  :config
  (defvar js-indent-level)
  (defvar json-reformat:indent-width)
  (defun cliffz-json-mode-indentation ()
    "Set json-mode indentation."
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2
          json-reformat:indent-width 2))
  (add-hook 'json-mode-hook 'cliffz-json-mode-indentation))

;; Javascript mode.
(use-package js2-mode
  :mode
  (("\\.js\\'" . js2-mode))
  :config
  (setq-default js2-basic-offset 4
                js-switch-indent-offset 4
                js-indent-level 4
                js2-strict-trailing-comma-warning nil)
  (set-face-attribute 'js2-error nil :underline '(:style line :color "#fb4934"))
  (set-face-attribute 'js2-warning nil :underline '(:style line :color "#fabd2f"))
  (set-face-attribute 'js2-external-variable nil :underline '(:style line :color "#b8bb26")))

(use-package rjsx-mode
  :mode
  (("\\.jsx\\'" . rjsx-mode))
  :config
  (setq-default js2-basic-offset 4
                js-switch-indent-offset 4
                js-indent-level 4
                js2-strict-trailing-comma-warning nil)
  (set-face-attribute 'js2-error nil :underline '(:style line :color "#fb4934"))
  (set-face-attribute 'js2-warning nil :underline '(:style line :color "#fabd2f"))
  (set-face-attribute 'js2-external-variable nil :underline '(:style line :color "#b8bb26")))

;; Javascript refactor.
(use-package js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :commands (js2r-add-keybindings-with-prefix)
  :config
  (js2r-add-keybindings-with-prefix "C-c m"))

;; Typescript mode.
(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode))

;; Typescript and javascript completion using tsserver.
(use-package tide
  :delight tide-mode
  :hook ((typescript-mode js2-mode rjsx-mode) . tide-setup)
  :init
  (defun cliffz-enable-tide-tsx ()
    "Enable tide for tsx files."
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (tide-setup)
      (flycheck-add-mode 'typescript-tslint 'web-mode)))
  (add-hook 'web-mode-hook 'cliffz-enable-tide-tsx)
  :config
  (defvar tide-tsserver-executable)
  (declare-function flycheck-mode "tide")
  (declare-function tide-hl-identifier-mode "tide")
  (declare-function company-mode "tide")
  (setq tide-tsserver-executable "~/.emacs.d/node_modules/typescript/bin/tsserver")
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Coffeescript mode.
(use-package coffee-mode
  :mode
  (("\\.coffee\\'" . coffee-mode))
  :config
  (defvar coffee-tab-width)
  (setq coffee-tab-width 2))

;; Add project node modules to path.
(use-package add-node-modules-path
    :hook ((typescript-mode js2-mode web-mode) . add-node-modules-path))

;; Web mode.
(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.tsx\\'" . web-mode))
  :config
  (defvar web-mode-markup-indent-offset)
  (defvar web-mode-css-indent-offset)
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 2))

;; Jade/Pug mode.
(use-package pug-mode
  :mode
  (("\\.jade\\'" . pug-mode))
  ("\\.pug\\'" . pug-mode))

;; Stylus mode.
(use-package stylus-mode
  :load-path "lisp/stylus-mode"
  :mode
  (("\\.styl\\'" . stylus-mode)))

;; Graphql mode.
(use-package graphql-mode
  :mode
  (("\\.graphql\\'" . graphql-mode)))

;; Markdown mode.
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

;; Dockerfile mode.
(use-package dockerfile-mode
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

;; Yaml mode.
(use-package yaml-mode
  :mode
  (("\\.yml\\'" . yaml-mode)))

;; Elastic search mode.
(use-package es-mode
  :mode
  (("\\.es\\'" . es-mode))
  :commands
  (es-command-center))

(provide 'init)

;;; init.el ends here
