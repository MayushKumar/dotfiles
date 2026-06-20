;;; prog.el --- Prog -*- lexical-binding: t; -*-
  (eval-when-compile (require 'bootstrap))


(setq-default c-basic-offset 4)
(setq-default c-default-style "bsd")

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'bsd)

(use-package lua-mode
  :mode "\\.lua\\'")

;; (use-package rust-ts-mode
;;   :mode "\\.rs\\'")

(use-package glsl-mode
  :mode ("\\.vert\\'"
         "\\.frag\\'"
         "\\.geom\\'"
         "\\.comp\\'"
         "\\.tesc\\'"
         "\\.tese\\'"
         "\\.glsl\\'"))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package latex
  :ensure
  (auctex
   :pre-build
   (("./autogen.sh")
    ("./configure" "--without-texmf-dir")
    ("make")))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-auto-save t)
  (TeX-engine 'luatex)
  (TeX-parse-self t)
  (TeX-master nil))

(use-package typst-ts-mode
  :mode "\\.typ\\'")

(use-package typst-preview
  :after typst-ts-mode
  :custom
  (typst-preview-invert-colors "never"))

(use-package kotlin-ts-mode
  :mode "\\.kt\\'"
  :config
  (add-to-list
   'treesit-language-source-alist
   '(kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))))

(use-package pyvenv
  :commands
  (pyvenv-activate
   pyvenv-workon
   pyvenv-deactivate))

(use-package kdl-ts-mode
  :ensure
  `(kdl-ts-mode
    :host github
    :repo "merrickluo/kdl-ts-mode")
  :mode "\\.kdl\\'"
  :config
  (add-to-list
   'treesit-language-source-alist
   '(kdl . ("https://github.com/tree-sitter-grammars/tree-sitter-kdl"))))

(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)

  :init
  (global-corfu-mode)

  :bind
  (:map corfu-map
        ("<tab>" . corfu-insert))

  :config
  (defun my/corfu-refresh ()
    (interactive)
    (when (bound-and-true-p corfu--frame)
      (corfu-quit))
    (completion-at-point)))

(use-package cape
  :init
  ;; Always request fresh completions from Eglot.
  (advice-add
   'eglot-completion-at-point
   :around
   #'cape-wrap-buster)

  ;; Additional completion source.
  (add-hook
   'completion-at-point-functions
   #'cape-file
   t))

(use-package nova
  :ensure
  (:host github
   :repo "thisisran/nova")
  :defer t)

 ;; (use-package treesit-auto
 ;;   :hook
 ;;   (after-init . global-treesit-auto-mode))

 ; apparently this is enough for emacs 31?
 (setq treesit-enabled-modes t)
;; (setq treesit-auto-install-grammar 'prompt)

(use-package eglot
  :defer t

  :custom
  (eglot-send-changes-idle-time 0.0)
  (eglot-ignored-server-capabilities
   '(:documentOnTypeFormattingProvider
     :inlayHintProvider
     :documentHighlightProvider))

  ;; :hook
  ;; ((c-mode
  ;;   c++-mode
  ;;   c-ts-mode
  ;;   c++-ts-mode
  ;;   kotlin-ts-mode)
  ;;  . eglot-ensure)

  :config
  (add-to-list
   'eglot-server-programs
   '((c-mode c++-mode c-ts-mode c++-ts-mode)
     . ("clangd"
        "--completion-style=detailed"
        "--header-insertion=never"
        "--limit-results=0")))

  (add-to-list
   'eglot-server-programs
   '(kotlin-ts-mode . ("kotlin-language-server")))

  :bind
  (:map eglot-mode-map
        ("C-c e a" . eglot-code-actions)
        ("C-c e r" . eglot-rename)
        ("C-c e f" . eglot-format)

        ("C-c e d" . xref-find-definitions)
        ("C-c e i" . eglot-find-implementation)
        ("C-c e t" . eglot-find-typeDefinition)
        ("C-c e u" . xref-find-references)

        ("C-c e h" . eldoc)
        ("C-c e s" . consult-eglot-symbols)

        ("C-c e l" . eglot-list-connections)
        ("C-c e q" . eglot-shutdown)
        ("C-c e R" . eglot-reconnect)))
(use-package consult-eglot
  :after eglot)

(use-package dape
  :ensure
  (:host github
   :repo "svaante/dape")

  :commands
  (dape
   dape-repl
   dape-breakpoint-toggle)

  :custom
  (dape-buffer-window-arrangement 'right)

  :config
  (dape-breakpoint-global-mode))

(provide 'prog)
