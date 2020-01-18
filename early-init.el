;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el, before package and UI initialization happens.

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+ `package-quickstart' precomputes autoloads file so that activation of packages can be done much faster.
(setq package-quickstart t)

;; Every file opened and loaded by Emacs will run through this list to check for a proper handler for the file, but
;; during startup, it won’t need any of them.
(defvar *file-name-handler-alist-original* file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing font. By inhibiting this, halve startup times
;; with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent loading of UI elements.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Theme Emacs title bar.
(push '(ns-transparent-titlebar . t) default-frame-alist)
(setq frame-title-format nil)

;; Set fringe width.
(fringe-mode '4)

;; Start Emacs maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'early-init)
