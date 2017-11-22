;; -*- mode: emacs-lisp -*-
;; This file contains custom theme options.

;; Whitespace-mode theme.
(set-face-attribute 'trailing-whitespace nil :background "#7c6f64")
;; Flyspell theme.
(set-face-attribute 'flyspell-duplicate nil :underline '(:style line :color "#fe8019"))
(set-face-attribute 'flyspell-incorrect nil :underline '(:style line :color "#fe8019"))
;; Flycheck theme.
(set-face-attribute 'flycheck-fringe-error nil :foreground "#fb4933")
(set-face-attribute 'flycheck-fringe-warning nil :foreground "#fabd2f")
(set-face-attribute 'flycheck-fringe-info nil :foreground "#83a598")
(set-face-attribute 'flycheck-error nil :underline '(:style line :color "#fb4933"))
(set-face-attribute 'flycheck-warning nil :underline '(:style line :color "#fabd2f"))
(set-face-attribute 'flycheck-info nil :underline '(:style line :color "#83a598"))
;; Spaceline theme.
(set-face-attribute 'powerline-active1 nil :background "#665c54")
(set-face-attribute 'powerline-active2 nil :background "#504945")
(set-face-attribute 'mode-line nil :background "#3c3836")
(set-face-attribute 'powerline-inactive1 nil :background "#665c54")
(set-face-attribute 'powerline-inactive2 nil :background "#504945")
(set-face-attribute 'mode-line-inactive nil :background "#3c3836")
(set-face-attribute 'spaceline-flycheck-error nil :foreground "#fb4933")
(set-face-attribute 'spaceline-flycheck-info nil :foreground "#fabd2f")
(set-face-attribute 'spaceline-flycheck-warning nil :foreground "#83a598")
(set-face-attribute 'spaceline-python-venv nil :foreground "#d3869b")
(set-face-attribute 'powerline-active1 nil :foreground "#fdf4c1")
(set-face-attribute 'powerline-active2 nil :foreground "#fdf4c1")
;; Diff-hl theme.
(set-face-attribute 'diff-hl-change nil :foreground "#83a598" :background "#282828")
(set-face-attribute 'diff-hl-delete nil :foreground "#fb4933")
(set-face-attribute 'diff-hl-insert nil :foreground "#b8bb26")
;; Js2 theme.
(add-hook 'js2-mode-hook
          '(lambda()
             (set-face-attribute 'js2-error nil :underline '(:style line :color "#fb4933"))
             (set-face-attribute 'js2-warning nil :underline '(:style line :color "#fabd2f"))
             (set-face-attribute 'js2-external-variable nil :underline '(:style line :color "#b8bb26"))))
