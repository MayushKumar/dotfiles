;;; orgconf.el --- OrgConf -*- lexical-binding: t; -*-
  (eval-when-compile (require 'bootstrap))


(use-package mixed-pitch
  :custom
  (mixed-pitch-set-height t))

(use-package org
  :mode ("\\.org\\'" . org-mode)

  :custom
  (org-startup-indented t)
  (org-image-actual-width nil)
  (org-pretty-entities t)
  (org-preview-latex-default-process 'dvisvgm)

  :config
  (require 'org-tempo)
  (add-to-list
   'org-structure-template-alist
   '("el" . "src emacs-lisp"))

  (add-to-list
   'org-latex-packages-alist
   '("" "chemfig" t))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)))

  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda ()
                (setq visual-fill-column-center-text t)
                (setq fill-column 140)
                (display-line-numbers-mode 0))))

(use-package org-modern
  :after org

  :custom
  (org-modern-block-indent t)
  (org-modern-hide-stars nil)

  (org-modern-todo-faces
   '(("STARTED" :foreground "yellow")
     ("CANCELED" org-special-keyword
      :inverse-video t
      :weight bold)))

  (org-modern-list
   '((?* . "•")
     (?+ . "‣")))

  (org-modern-checkbox
   '((?X . "✔")
     (?- . "┅")
     (?\s . " ")))

  (org-modern-label-border 1)

  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-roam
  :after org
  :custom
  (org-roam-v2-ack t))

(use-package ox-gfm
  :after org)

(provide 'orgconf)
