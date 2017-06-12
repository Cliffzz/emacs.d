;;; packages.el --- graphql layer packages file for Spacemacs.
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst graphql-packages
  '(graphql-mode))

(defun graphql/init-graphql-mode ()
    (use-package graphql-mode
      :defer t
      :init
      (progn
        (spacemacs/set-leader-keys-for-major-mode 'graphql-mode
          "s" 'graphql-send-query
          ))))

;;; packages.el ends here
