;;; core-terminal.el -*- lexical-binding: t; -*-


;; Don't pause the output through $PAGER
(setenv "PAGER" "cat")

;; Suppress compiler warning.
(defvar eshell-history-size)
(defvar eshell-plain-echo-behavior)
(defvar eshell-save-history-on-exit)
(defvar eshell-buffer-maximum-lines)
(defvar eshell-truncate-timer)
(defvar eshell-hist-ignoredups)
(defvar eshell-scroll-to-bottom-on-input)
(defvar eshell-prompt-function)
(defvar tramp-remote-path)
(declare-function eshell-truncate-buffer "core-terminal")

;; Asynchronously truncate terminal buffer when `eshell-buffer-maximum-lines' is reached instead of every line.
(setq eshell-buffer-maximum-lines 10000)

(defun truncate-eshell-buffers ()
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))

(setq eshell-truncate-timer
      (run-with-idle-timer 5 t #'truncate-eshell-buffers))

;; Save as many commands in history as possible and avoid duplicates.
(setq eshell-history-size 100000
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t)

;; Let echo behave more like an ordinary shell echo.
(setq eshell-plain-echo-behavior t)

;; Automatcially scroll terminal to bottom on input.
(setq eshell-scroll-to-bottom-on-input 't)

;; Add `clear' command to terminal that clears the entire terminal buffer.
(defun eshell/clear ()
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Set remote path as `TRAMP' path, after `TRAMP' has been loaded.
(defun set-tramp-remote-path ()
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(add-hook 'tramp-mode-hook #'set-tramp-remote-path)

;; Easily open a new terminal.
(use-package shell-pop
  :bind
  (("C-c t" . 'shell-pop))
  :config
  (declare-function shell-pop--set-shell-type "shell-pop")
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))
        shell-pop-full-span t
        shell-pop-window-size 25)
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; Display extra information in terminal prompt.
(use-package eshell-prompt-extras
  :after
  (esh-opt)
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-prompt-function 'epe-theme-lambda))

;; Move up the path to target instead of multiple cd.
(use-package eshell-up
  :commands
  (eshell-up)
  :init
  (defun eshell/up ($1)
    (eshell-up $1)))

;; Node version management.
(use-package nvm
  :commands
  (nvm-use)
  :init
  (defun eshell/nvm-use ($1)
    (nvm-use (number-to-string $1))
    (eshell-mode)
    (eshell/clear)))

(provide 'core-terminal)
;;; core-terminal.el ends here
