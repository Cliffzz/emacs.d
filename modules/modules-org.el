;;; modules-org.el -*- lexical-binding: t; -*-

;; Major mode for editing org files.
(use-package org
  :ensure org-plus-contrib
  :mode
  (("\\.org\\'" . org-mode))
  :bind
  (("C-c o l" . 'org-store-link)
   ("C-c o a" . 'org-agenda)
   ("C-c o c" . 'org-capture))
  :config
  (setq org-directory (concat user-emacs-directory "org")
        org-default-notes-file (concat org-directory "/refile.org")
        org-agenda-files (list org-directory)
        org-log-done 'time
        org-clock-out-remove-zero-time-clocks t
        org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                            (sequence "MEETING"))
        org-capture-templates '(("t" "Todo" entry (file org-default-notes-file)
                                 "* TODO %?\n  %i\n  %a" :clock-in t :clock-resume t)
                                ("m" "Meeting" entry (file org-default-notes-file)
                                 "* MEETING %?\n  %i\n  %a" :clock-in t :clock-resume t)
                                ("j" "Journal" entry (file org-default-notes-file)
                                 "* %?\n  %i\n  %a" :clock-in t :clock-resume t))
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 1)))))

;; Org bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'modules-org)
;;; modules-org.el ends here
