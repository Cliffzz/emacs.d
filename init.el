;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Cliffzz
;; URL:     https://github.com/Cliffzz/.emacs.d
;;
;;; License: GPL


;; Set `gc-cons-threshold' to a reasonable value. A large `gc-cons-threshold' may cause freezing and stuttering during
;; long-term interactive use.
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

;; Restore original file name handler list that was removed in `early-init.el'.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist *file-name-handler-alist-original*)
            (makunbound 'file-name-handler-alist-original)))

;; Move Custom-Set-Variables to seperate file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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

;; Force spaces for indentation.
(setq-default indent-tabs-mode nil)

;; Highlight current line.
(global-hl-line-mode 1)

;; Truncate lines.
(setq-default truncate-lines t)

;; Mitigate performance problems with long lines.
(global-so-long-mode)

;; Highlight parentheses.
(show-paren-mode t)

;; Add a newline automatically at the end of the file.
(setq require-final-newline t)

;; Disable backup files.
(setq make-backup-files nil)

;; Disable file locks.
(setq create-lockfiles nil)

;; Disable auto sae files.
(setq auto-save-default nil)

;; Enable tabs.
(global-tab-line-mode)

;; Line numbers.
(setq-default display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Enable fill column indicator and set to width of 120.
(setq-default display-fill-column-indicator-column 120)
(global-display-fill-column-indicator-mode)

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
