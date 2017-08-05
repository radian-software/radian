;;; radian-pairs.el --- Paired delimiter handling

(require 'radian-package)

(use-package smartparens
  :init

  (require 'smartparens-config)
  (smartparens-global-strict-mode +1)
  (sp-use-paredit-bindings)

  :diminish smartparens-mode)

(provide 'radian-pairs)

;;; radian-pairs.el ends here
