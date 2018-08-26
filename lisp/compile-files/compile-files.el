;;; compile-files.el --- Compile files. -*-no-byte-compile: t; -*-


;;; Commentary:
;;
;; Compile all config files.
;;
;;; Code:

(defun compile-files ()
  "Compile all config files."
  (interactive)
  (byte-compile-file "~/.emacs.d/init.el")
  (byte-compile-file "~/.emacs.d/custom-settings.el")
  (byte-compile-file "~/.emacs.d/lisp/stylus-mode/stylus-mode.el")
  (byte-compile-file "~/.emacs.d/lisp/wucuo/wucuo.el"))

(provide 'compile-files)

;;; compile-files.el ends here
