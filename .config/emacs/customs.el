(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
	 ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
	 (output-pdf "Zathura") (output-html "xdg-open")))
 '(base16-theme-distinct-fringe-background nil)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
	  tramp-kubernetes-connection-local-default-profile)
	 ((:application tramp :protocol "flatpak")
	  tramp-container-connection-local-default-flatpak-profile
	  tramp-flatpak-connection-local-default-profile)
	 ((:application tramp)
	  tramp-connection-local-default-system-profile
	  tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
	  (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
						 "/usr/bin" "/sbin" "/usr/sbin"
						 "/usr/local/bin" "/usr/local/sbin"
						 "/local/bin" "/local/freeware/bin"
						 "/local/gnu/bin" "/usr/freeware/bin"
						 "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
						 "/opt/sbin" "/opt/local/bin"))
	 (tramp-kubernetes-connection-local-default-profile
	  (tramp-config-check . tramp-kubernetes--current-context-data)
	  (tramp-extra-expand-args 97
							   (tramp-kubernetes--container
								(car tramp-current-connection))
							   104
							   (tramp-kubernetes--pod
								(car tramp-current-connection))
							   120
							   (tramp-kubernetes--context-namespace
								(car tramp-current-connection))))
	 (tramp-container-connection-local-default-flatpak-profile
	  (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
						 "/usr/bin" "/sbin" "/usr/sbin"
						 "/usr/local/bin" "/usr/local/sbin"
						 "/local/bin" "/local/freeware/bin"
						 "/local/gnu/bin" "/usr/freeware/bin"
						 "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
						 "/opt/sbin" "/opt/local/bin"))
	 (tramp-connection-local-darwin-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o"
										"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o" "state=abcde" "-o"
										"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format (pid . number)
										  (euid . number)
										  (user . string)
										  (egid . number) (comm . 52)
										  (state . 5) (ppid . number)
										  (pgrp . number)
										  (sess . number)
										  (ttname . string)
										  (tpgid . number)
										  (minflt . number)
										  (majflt . number)
										  (time . tramp-ps-time)
										  (pri . number)
										  (nice . number)
										  (vsize . number)
										  (rss . number)
										  (etime . tramp-ps-time)
										  (pcpu . number)
										  (pmem . number) (args)))
	 (tramp-connection-local-busybox-ps-profile
	  (tramp-process-attributes-ps-args "-o"
										"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o" "stat=abcde" "-o"
										"ppid,pgid,tty,time,nice,etime,args")
	  (tramp-process-attributes-ps-format (pid . number)
										  (user . string)
										  (group . string) (comm . 52)
										  (state . 5) (ppid . number)
										  (pgrp . number)
										  (ttname . string)
										  (time . tramp-ps-time)
										  (nice . number)
										  (etime . tramp-ps-time)
										  (args)))
	 (tramp-connection-local-bsd-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o"
										"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o"
										"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format (pid . number)
										  (euid . number)
										  (user . string)
										  (egid . number)
										  (group . string) (comm . 52)
										  (state . string)
										  (ppid . number)
										  (pgrp . number)
										  (sess . number)
										  (ttname . string)
										  (tpgid . number)
										  (minflt . number)
										  (majflt . number)
										  (time . tramp-ps-time)
										  (pri . number)
										  (nice . number)
										  (vsize . number)
										  (rss . number)
										  (etime . number)
										  (pcpu . number)
										  (pmem . number) (args)))
	 (tramp-connection-local-default-shell-profile
	  (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
	 (tramp-connection-local-default-system-profile
	  (path-separator . ":") (null-device . "/dev/null"))))
 '(custom-enabled-themes '(ef-duo-dark))
 '(eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider :inlayHintProvider))
 '(inhibit-startup-screen t)
 '(safe-local-variable-directories '("/home/mayush/dev/Noor/sandbox/")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-items-face ((t (:inherit widget-button :weight medium)))))
