;;; init.el --- Main entry point -*- lexical-binding: t; -*-

;; Add our modules directory to the load path.
(add-to-list
 'load-path
 (expand-file-name "lisp" user-emacs-directory))

;; Load configuration modules in dependency order.
(dolist (module
         '(bootstrap
           core
		   prog
           orgconf
		   ))
  (require module))

;; Optional machine-specific configuration.
(let ((local (expand-file-name "lisp/local.el"
                               user-emacs-directory)))
  (when (file-exists-p local)
    (load local nil t)))

(provide 'init)

;;; init.el ends here
