;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(nordic-midnight))
 '(safe-local-variable-values
   '((eval add-hook 'org-babel-post-tangle-hook #'my/recompile-config nil
		   t)
	 (dape-configs
	  (sandbox-codelldb modes (c-mode c-ts-mode c++-mode c++-ts-mode)
						command-args ("--port" :autoport) ensure
						dape-ensure-command command-cwd
						dape-command-cwd command
						"~/.config/emacs/debug-adapters/codelldb/extension/adapter/codelldb"
						port :autoport :type "lldb" :request "launch"
						:cwd "sandbox/" :program
						"build/sandbox/Debug/sandbox" :args []
						:stopOnEntry nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
