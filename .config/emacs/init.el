(setq custom-safe-themes t)
(setq base16-distinct-fringe-background nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(base16-gruvbox-material-dark-medium))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(tramp sudo-edit eglot consult-lsp yaml-mode pdf-tools rainbow-mode solarized-theme yasnippet winum which-key visual-fill-column vertico use-package treemacs-evil tree-sitter-langs telephone-line shrink-path ripgrep rainbow-delimiters projectile org orderless marginalia magit lua-mode lsp-ui helpful glsl-mode flycheck evil-commentary evil-collection doom-themes dashboard dap-mode corfu consult company-box cmake-mode base16-theme all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-file "~/.config/emacs/emacsinit.el")
