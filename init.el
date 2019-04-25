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

;; Themed title bar. (macOS only)
(cond ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
       (add-to-list 'default-frame-alist '(ns-appearance . dark))
       (setq frame-title-format nil)))

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
  (setq network-security-level 'high))

;; Smooth scrolling.
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

;; Horizontal scrolling.
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Font setup.
(defvar cliffz-font-size 130)
(cond ((eq system-type 'windows-nt)
       (setq cliffz-font-size 100)))

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height cliffz-font-size
                    :weight 'normal
                    :width 'normal)

;; Set fringe size
(fringe-mode '0)
(defun cliffz-enable-fringe ()
  "Enable fringes."
  (setq left-fringe-width 4
        right-fringe-width 4))
(add-hook 'prog-mode-hook 'cliffz-enable-fringe)
(add-hook 'text-mode-hook 'cliffz-enable-fringe)

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

;; Backups.
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 12
      kept-old-versions 4)

;; Disable package.el initialization by default.
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Make sure elpa directory exists.
(eval-when-compile
  (unless (file-directory-p "~/.emacs.d/elpa")
    (make-directory "~/.emacs.d/elpa" t)))

;; Initialize load-path for packages.
(setq load-path (eval-when-compile (append load-path (directory-files "~/.emacs.d/elpa" t "^[^.]" t))))
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/doom-themes-20190109.538")
(autoload #'use-package-autoload-keymap "use-package")

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

;; Keep emacs.d clean
(use-package no-littering
  :demand t
  :commands no-littering-expand-var-file-name
  :init
  (setq no-littering-etc-directory
        (expand-file-name ".cache/etc" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name ".cache/var" user-emacs-directory))
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Change mode line names.
(use-package delight
  :demand t)

(use-package eldoc
  :commands eldoc-mode
  :delight eldoc-mode)

;; Dependency for custom keybinds.
(use-package bind-key
  :demand t)

;; Elpa mirror.
(use-package elpa-mirror
  :commands (elpamr-create-mirror-for-installed))

;; Bind compile files command.
(use-package compile-files
  :load-path "lisp/compile-files"
  :commands (compile-files)
  :init
  (bind-key "C-c c" 'compile-files emacs-lisp-mode-map))

;; Fix path variables in macOS.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :commands (exec-path-from-shell-initialize)
  :config
  (setq exec-path
        (or (eval-when-compile
              (when (require 'exec-path-from-shell nil t)
                (cond ((eq system-type 'darwin)
                       (setq exec-path-from-shell-check-startup-files nil
                             exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
                       (exec-path-from-shell-initialize)))
                exec-path))
            exec-path)))

;; TRAMP
(use-package tramp
  :init
  (cond ((eq system-type 'windows-nt)
         (defvar tramp-default-method)
         (setq tramp-default-method "plink")))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Dashboard
(use-package dashboard
  :defer nil
  :commands (dashboard-refresh-buffer dashboard-setup-startup-hook)
  :if (< (length command-line-args) 2)
  :preface
  (defun cliffz-dashboard-banner ()
    "Sets a dashboard banner including information on package initialization
     time and garbage collections."
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time
                   (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'cliffz-dashboard-banner)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects  . 15)
                          (recents . 15))))

;; Delete trailing whitespaces on save.
(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :init
  (defun cliffz-show-trailing-whitespace ()
    "Show trailing whitespace."
    (setq show-trailing-whitespace 1))
  (add-hook 'prog-mode-hook 'cliffz-show-trailing-whitespace))

;; Spell checking.
(use-package wucuo
  :disabled t
  :hook
  (prog-mode . wucuo-start)
  (text-mode . wucuo-start)
  :delight flyspell-mode
  :init
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  :config
  (set-face-attribute 'flyspell-incorrect nil :underline '(:style line :color "#ff6c6b"))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:style line :color "#ecBe7b")))

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
  (set-face-attribute 'flycheck-error nil :underline '(:style line :color "#ff6c6b"))
  (set-face-attribute 'flycheck-warning nil :underline '(:style line :color "#ecBe7b"))
  (set-face-attribute 'flycheck-info nil :underline '(:style line :color "#98be65")))
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

;; Keybinds auto completion.
(use-package which-key
  :delight which-key-mode
  :commands (which-key-mode)
  :init
  (which-key-mode))

;; Code auto completion.
(use-package company
  :delight company-mode
  :hook (prog-mode . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-backends '()))

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
  :commands projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (defvar projectile-sort-order)
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy)
  (projectile-mode +1))

;; All the icons
(use-package all-the-icons
  :init
  (setq inhibit-compacting-font-caches t))

;; Mode line setup.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq line-number-mode t
        column-number-mode t
        doom-modeline-icon t
        doom-modeline-height 30))

;; Praise the sun.
(use-package solaire-mode
  :demand t
  :commands solaire-global-mode solaire-mode-swap-bg
  :init
  (defun cliffz-init-solaire-mode (frame)
    "Init solaire-mode on a new frame."
    (with-selected-frame frame
      (solaire-global-mode)))
  (add-hook 'after-make-frame-functions 'cliffz-init-solaire-mode))

;; Doom themes
(use-package doom-themes
  :demand t
  :commands doom-themes-visual-bell-config doom-themes-neotree-config
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (defun cliffz-init-doom-theme (frame)
    "Init doom modeline on a new frame."
    (with-selected-frame frame
      (load-theme 'doom-one t)
      (doom-themes-visual-bell-config)
      (solaire-mode-swap-bg)))
  (add-hook 'after-make-frame-functions 'cliffz-init-doom-theme)
  (if (not (daemonp))
      (progn
        (load-theme 'doom-one t)
        (doom-themes-visual-bell-config)
        (solaire-global-mode)
        (solaire-mode-swap-bg))))

;; File explorer
(use-package neotree
  :commands neotree-toggle
  :bind (("C-x t" . 'neotree-toggle))
  :config
  (defun cliffz-hide-neotree-modeline ()
    "Hide neotree modeline."
    (setq-local mode-line-format nil))
  (add-hook 'neotree-mode-hook 'cliffz-hide-neotree-modeline)
  (defvar doom-neotree-file-icons)
  (setq doom-neotree-file-icons t)
  (setq neo-window-width 40)
  (setq neo-show-hidden-files t)
  (doom-themes-neotree-config))

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
  :bind (("C-c S" . 'ripgrep-regexp)))

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
  (defvar eshell-save-history-on-exit)
  (setq eshell-cmpl-cycle-completions nil
        eshell-buffer-maximum-lines 20000
        eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-buffer-shorthand t
        eshell-highlight-prompt nil
        eshell-plain-echo-behavior t
        eshell-scroll-to-bottom-on-input 'all)

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)))

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

;; Shell pop.
(use-package shell-pop
  :commands (shell-pop)
  :bind (("C-c t" . 'shell-pop))
  :config
  (declare-function shell-pop--set-shell-type "shell-pop")
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))
        shell-pop-full-span t
        shell-pop-window-size 25)
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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
  :commands (avy-goto-char-timer)
  :bind (("C-'" . 'avy-goto-char-timer)))

;; Jump to window.
(use-package winum
  :hook (after-init . winum-mode)
  :config
  (declare-function winum-assign-0-to-neotree "init")
  (defun winum-assign-0-to-neotree ()
    "Assign neotree to window 10."
    (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
  (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree))

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

;; Org bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

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
                js2-strict-trailing-comma-warning nil
                js2-include-node-externs t))

(use-package rjsx-mode
  :mode
  (("\\.jsx\\'" . rjsx-mode))
  :config
  (setq-default js2-basic-offset 4
                js-switch-indent-offset 4
                js-indent-level 4
                js2-strict-trailing-comma-warning nil))

;; Javascript refactor.
(use-package js2-refactor
  :delight js2-refactor-mode
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :commands (js2r-add-keybindings-with-prefix cliffz-set-refactor-keybind-descriptions)
  :config
  (declare-function which-key-add-major-mode-key-based-replacements "which-key")
  (defun cliffz-set-refactor-keybind-descriptions (mode)
    "Set the refactor keybind descriptions for the given mode."
    (which-key-add-major-mode-key-based-replacements mode
      "C-c r" "refactor"

      "C-c r 3" "ternary"
      "C-c r a" "add/args"
      "C-c r b" "barf"
      "C-c r c" "contract"
      "C-c r e" "expand/extract"
      "C-c r i" "inline/inject/introduct"
      "C-c r l" "localize/log"
      "C-c r r" "rename"
      "C-c r s" "split/slurp"
      "C-c r t" "toggle"
      "C-c r u" "unwrap"
      "C-c r v" "var"
      "C-c r w" "wrap"
      "C-c r t" "text"))

  (js2r-add-keybindings-with-prefix "C-c r")
  (cliffz-set-refactor-keybind-descriptions 'js2-mode)
  (cliffz-set-refactor-keybind-descriptions 'rjsx-mode))

;; Typescript mode.
(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode))

;; Typescript and javascript completion using tsserver.
(use-package tide
  :delight tide-mode
  :commands tide-setup
  :init
  (defun cliffz-setup-tide ()
    "Setup tide mode."
    (defvar tide-tsserver-executable)
    (declare-function flycheck-mode "tide")
    (declare-function tide-hl-identifier-mode "tide")
    (declare-function company-mode "tide")
    (setq tide-tsserver-executable "~/.emacs.d/node_modules/typescript/bin/tsserver")
    (setq tide-completion-detailed t)
    (tide-setup)
    (eldoc-mode +1)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode +1)
    (make-local-variable 'company-backends)
    (setq company-backends '((company-tide company-dabbrev-code))))
  (add-hook 'js2-mode-hook 'cliffz-setup-tide)
  (add-hook 'rjsx-mode-hook 'cliffz-setup-tide)
  (add-hook 'typescript-mode-hook 'cliffz-setup-tide)
  (defun cliffz-enable-tide-tsx ()
    "Enable tide for tsx files."
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (declare-function cliffz-setup-tide "init")
      (cliffz-setup-tide)
      (flycheck-add-mode 'typescript-tslint 'web-mode)))
  (add-hook 'web-mode-hook 'cliffz-enable-tide-tsx))

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

;; Node version management.
(use-package nvm
  :if (file-exists-p "~/.nvm")
  :commands (nvm-use)
  :init
  (defun cliffz-nvm-use (version)
    (interactive "sVersion: ")
    (nvm-use version))
  (declare-function cliffz-set-nvm-keybind-descriptions "init")
  (defun cliffz-set-nvm-keybind-descriptions (mode)
    "Set the nvm-use keybind description for the given mode."
    (which-key-add-major-mode-key-based-replacements mode
      "C-c n" "nvm use"))
  (defun cliffz-set-nvm-keybinds-js2-mode ()
    "Set nvm keybinds for js2-mode"
    (bind-key "C-c n" 'cliffz-nvm-use js2-mode-map)
    (cliffz-set-nvm-keybind-descriptions 'js2-mode))
  (defun cliffz-set-nvm-keybinds-rjsx-mode ()
    "Set nvm keybinds for rjsx-mode"
    (bind-key "C-c n" 'cliffz-nvm-use rjsx-mode-map)
    (cliffz-set-nvm-keybind-descriptions 'rjsx-mode))
  (defun cliffz-set-nvm-keybinds-typescript-mode ()
    "Set nvm keybinds for typescript-mode"
    (bind-key "C-c n" 'cliffz-nvm-use typescript-mode-map)
    (cliffz-set-nvm-keybind-descriptions 'typescript-mode))
  (add-hook 'js2-mode-hook 'cliffz-set-nvm-keybinds-js2-mode)
  (add-hook 'rjsx-mode-hook 'cliffz-set-nvm-keybinds-rjsx-mode)
  (add-hook 'typescript-mode-hook 'cliffz-set-nvm-keybinds-typescript-mode))

;; Javascript debugger.
(use-package indium
  :commands (indium-connect-to-nodejs cliffz-set-indium-keybind-descriptions)
  :init
  (declare-function which-key-add-major-mode-key-based-replacements "which-key")
  (defun cliffz-set-indium-keybind-descriptions (mode)
    "Set the indium keybind descriptions for the given mode."
    (which-key-add-major-mode-key-based-replacements mode
      "C-c d" "debugger"))

  (defun cliffz-set-indium-keybinds-js2-mode ()
    "Set the indium debugger keybinds for js2-mode"
    (bind-key "C-c d n" 'indium-connect-to-nodejs js2-mode-map)
    (bind-key "C-c d c" 'indium-connect-to-chrome js2-mode-map)
    (cliffz-set-indium-keybind-descriptions 'js2-mode))
  (defun cliffz-set-indium-keybinds-rjsx-mode ()
    "Set the indium debugger keybinds for rjsx-mode"
    (bind-key "C-c d n" 'indium-connect-to-nodejs rjsx-mode-map)
    (bind-key "C-c d c" 'indium-connect-to-chrome rjsx-mode-map)
    (cliffz-set-indium-keybind-descriptions 'rjsx-mode))
  (defun cliffz-set-indium-keybinds-typescript-mode ()
    "Set the indium debbuger  keybinds for typescript-mode"
    (bind-key "C-c d n" 'indium-connect-to-nodejs typescript-mode-map)
    (bind-key "C-c d c" 'indium-connect-to-chrome typescript-mode-map)
    (cliffz-set-indium-keybind-descriptions 'typescript-mode))

  (add-hook 'js2-mode-hook 'cliffz-set-indium-keybinds-js2-mode)
  (add-hook 'rjsx-mode-hook 'cliffz-set-indium-keybinds-rjsx-mode)
  (add-hook 'typescript-mode-hook 'cliffz-set-indium-keybinds-typescript-mode))

;; Mocha test runner.
(use-package mocha
  :commands (mocha-test-file mocha-test-at-point mocha-test-project cliffz-set-mocha-keybind-descriptions)
  :init
  (declare-function which-key-add-major-mode-key-based-replacements "which-key")
  (defun cliffz-set-mocha-keybind-descriptions (mode)
    "Set the mocha keybind descriptions for the given mode."
    (which-key-add-major-mode-key-based-replacements mode
      "C-c m" "mocha"))
  (defun cliffz-set-mocha-keybinds-js2-mode ()
    "Set the mocha keybinds for js2-mode"
    (bind-key "C-c m f" 'mocha-test-file js2-mode-map)
    (bind-key "C-c m p" 'mocha-test-at-point js2-mode-map)
    (bind-key "C-c m P" 'mocha-test-project js2-mode-map)
    (cliffz-set-mocha-keybind-descriptions 'js2-mode))
  (defun cliffz-set-mocha-keybinds-rjsx-mode ()
    "Set the mocha keybinds for rjsx-mode"
    (bind-key "C-c m f" 'mocha-test-file rjsx-mode-map)
    (bind-key "C-c m p" 'mocha-test-at-point rjsx-mode-map)
    (bind-key "C-c m P" 'mocha-test-project rjsx-mode-map)
    (cliffz-set-mocha-keybind-descriptions 'rjsx-mode))
  (defun cliffz-set-mocha-keybinds-typescript-mode ()
    "Set the mocha keybinds for typescript-mode"
    (bind-key "C-c m f" 'mocha-test-file typescript-mode-map)
    (bind-key "C-c m p" 'mocha-test-at-point typescript-mode-map)
    (bind-key "C-c m P" 'mocha-test-project typescript-mode-map)
    (cliffz-set-mocha-keybind-descriptions 'typescript-mode))
  (add-hook 'js2-mode-hook 'cliffz-set-mocha-keybinds-js2-mode)
  (add-hook 'rjsx-mode-hook 'cliffz-set-mocha-keybinds-rjsx-mode)
  (add-hook 'typescript-mode-hook 'cliffz-set-mocha-keybinds-typescript-mode))

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

;; Prettier, code formatting.
(use-package prettier-js
  :commands (prettier-js)
  :init
  (defvar graphql-mode-map)
  (defvar yaml-mode-map)

  (cond ((eq system-type 'windows-nt)
         (setq prettier-js-command "~/.emacs.d/node_modules/.bin/prettier.cmd")))
  (cond ((eq system-type 'darwin)
         (setq prettier-js-command "~/.emacs.d/node_modules/.bin/prettier")))
  (setq prettier-js-args '("--print-width" "100" "--tab-width" "4" "--single-quote" "--trailing-comma" "all"))

  (defun cliffz-set-prettier-js-config-js2-mode ()
    "Set the prettier-js config for js2-mode"
    (make-local-variable 'prettier-js-command)
    (cond ((eq system-type 'windows-nt)
           (setq prettier-js-command "~/.emacs.d/node_modules/.bin/prettier-eslint.cmd")))
    (cond ((eq system-type 'darwin)
           (setq prettier-js-command "~/.emacs.d/node_modules/.bin/prettier-eslint")))
    (bind-key "C-c f" 'prettier-js js2-mode-map))
  (defun cliffz-set-prettier-js-config-json-mode ()
    "Set the prettier-js keybind for json-mode."
    (make-local-variable 'prettier-js-args)
    (setq prettier-js-args '("--print-width" "100" "--tab-width" "2" "--single-quote" "--trailing-comma" "all"))
    (bind-key "C-c f" 'prettier-js json-mode-map))
  (defun cliffz-set-prettier-js-config-rjsx-mode ()
    "Set the prettier-js config for rjsx-mode."
    (make-local-variable 'prettier-js-command)
    (cond ((eq system-type 'windows-nt)
           (setq prettier-js-command "~/.emacs.d/node_modules/.bin/prettier-eslint.cmd")))
    (cond ((eq system-type 'darwin)
           (setq prettier-js-command "~/.emacs.d/node_modules/.bin/prettier-eslint")))
    (bind-key "C-c f" 'prettier-js rjsx-mode-map))
  (defun cliffz-set-prettier-js-config-typescript-mode ()
    "Set the prettier-js keybind for typescript-mode."
    (bind-key "C-c f" 'prettier-js typescript-mode-map))
  (defun cliffz-set-prettier-js-config-graphql-mode ()
    "Set the prettier-js keybind for graphql-mode."
    (bind-key "C-c f" 'prettier-js graphql-mode-map))
  (defun cliffz-set-prettier-js-config-yaml-mode ()
    "Set the prettier-js keybind for yaml-mode."
    (make-local-variable 'prettier-js-args)
    (setq prettier-js-args '("--print-width" "100" "--tab-width" "2" "--single-quote" "--trailing-comma" "all"))
    (bind-key "C-c f" 'prettier-js yaml-mode-map))
  (add-hook 'js2-mode-hook 'cliffz-set-prettier-js-config-js2-mode)
  (add-hook 'json-mode-hook 'cliffz-set-prettier-js-config-json-mode)
  (add-hook 'rjsx-mode-hook 'cliffz-set-prettier-js-config-rjsx-mode)
  (add-hook 'typescript-mode-hook 'cliffz-set-prettier-js-config-typescript-mode)
  (add-hook 'graphql-mode-hook 'cliffz-set-prettier-js-config-graphql-mode)
  (add-hook 'yaml-mode-hook 'cliffz-set-prettier-js-config-yaml-mode))

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
  (("\\.graphql\\'" . graphql-mode))
  :config
  (setq graphql-indent-level 4))

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

;; Lua mode.
(use-package lua-mode
  :mode
  (("\\.lua\\'" . lua-mode)))

;; Lua autocompletion.
(use-package company-lua
  :after (lua-mode)
  :init
  (defun cliffz-load-lua-autocompletion ()
    "Load lua auto completion."
    (make-local-variable 'company-backends)
    (setq company-backends '((company-lua company-dabbrev-code))))
  (add-hook 'lua-mode-hook 'cliffz-load-lua-autocompletion))

;; Gherkin / cucumber mode.
(use-package feature-mode
  :mode
  (("\\.feature\\'" . feature-mode)))

(provide 'init)

;;; init.el ends here
