;;; modules.el -*- lexical-binding: t; -*-

;; Ensure `modules-dir' is in `load-path'.
(add-to-list 'load-path (file-name-directory load-file-name))

(defun initialize-modules ()
  (require 'modules-json)
  (require 'modules-javascript))

(provide 'modules)
;;; modules.el ends here
