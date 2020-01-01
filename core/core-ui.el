;;; core-ui.el -*- lexical-binding: t; -*-

;; Set `gc-cons-threshold' to a reasonable value after Emacs startup is complete. A large `gc-cons-threshold' may cause
;; freezing and stuttering during long-term interactive use.
(defvar *default-garbage-collection-threshold* 16777216)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold *default-garbage-collection-threshold*)))

;; Raise `gc-cons-threshold' while the minibuffer is active, so the GC doesn’t slow down expensive commands or
;; completion frameworks. Defer `restore-garbage-collection-hook' so that commands launched immediately after will enjoy
;; the benefits.
(defun defer-garbage-collection-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-hook ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold *default-garbage-collection-threshold*))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-hook)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-hook)

;; Disable startup message.
(setq inhibit-startup-message t)

;; Disable default welcome message.
(defun display-startup-echo-area-message ()
  (message nil))

;; Disable cursor alarms.
(setq ring-bell-function 'ignore)

;; Disable inital scratch message.
(setq initial-scratch-message nil)

;; Start scratch in text-mode.
(setq initial-major-mode 'text-mode)

;; Always blink cursor.
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)

;; Simplify yes/no prompts to y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight current line.
(global-hl-line-mode 1)

;; Truncate lines.
(setq-default truncate-lines t)

;; Highlight parentheses.
(show-paren-mode t)

;; Line numbers.
(setq-default display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Enable fill column indicator and set to width of 120.
(setq-default display-fill-column-indicator-column 120)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Horizontal scrolling.
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

;; Smooth vertical scrolling.
(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; Smooth horizontal scrolling.
(setq hscroll-step 1
      hscroll-margin 1)

;; Default font.
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Show trailing whitespace.
(defun show-trailing-whitespace ()
  (setq show-trailing-whitespace 1))
(add-hook 'prog-mode-hook #'show-trailing-whitespace)

;; Icon fonts for Emacs.
(use-package all-the-icons
  :commands
  (all-the-icons-install-fonts))

;; Display the key bindings following an incomplete command.
(use-package which-key
  :init
  (which-key-mode))

;; Colorize parentheses.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Dashboard
(use-package dashboard
  :if (< (length command-line-args) 2)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-footer (concat "Version: " emacs-version)
        dashboard-items '((projects  . 15)
                          (recents . 15))))

;; Setup main theme.
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

;; Setup modeline.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Help to visually distinguish file-visiting windows from other types of windows by giving them a slightly different
;; -- often brighter -- background.
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; Syntax checking theme.
(use-package flycheck
  :defer t
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  (set-face-attribute 'flycheck-error nil :underline '(:style line :color "#ff6c6b"))
  (set-face-attribute 'flycheck-warning nil :underline '(:style line :color "#ecBe7b"))
  (set-face-attribute 'flycheck-info nil :underline '(:style line :color "#98be65")))

;; Thin git fringe theme.
(use-package git-gutter-fringe
  :defer t
  :config
  (setq git-gutter-fr:side 'left-fringe)
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(provide 'core-ui)
;;; core-ui.el ends here
