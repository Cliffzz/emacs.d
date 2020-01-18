;;; modules-restclient.el -*- lexical-binding: t; -*-

;; HTTP REST client tool for emacs
(use-package restclient
  :mode
  (("\\.restclient\\'" . restclient-mode)))

;; Restclient autocompletion.
(use-package company-restclient
  :commands (company-restclient))

;; Enable restclient completion.
(defun setup-restclient-completion ()
  (defvar company-backends)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-restclient)))

(add-hook 'restclient-mode-hook #'setup-restclient-completion)

(provide 'modules-restclient)
;;; modules-restclient.el ends here
