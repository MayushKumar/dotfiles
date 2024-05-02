(setq  gc-cons-threshold most-positive-fixnum
	   package-enable-at-startup nil)

(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

;; LSP Booster
(setenv "LSP_USE_PLISTS" "true")
