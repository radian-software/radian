;;; radian-check.el --- On-the-fly syntax and semantics checking

(require 'radian-package)

(use-package flycheck
  :init

  ;; Enable Flycheck everywhere unless otherwise specified.
  (global-flycheck-mode)

  :diminish flycheck-mode)

(provide 'radian-check)

;;; radian-check.el ends here
