;;; radian-check.el --- On-the-fly syntax and semantics checking

(require 'radian-package)

(use-package flycheck
  :init

  ;; Enable Flycheck everywhere unless otherwise specified.
  (global-flycheck-mode)

  :config

  ;; Make it safe to set the Python executable to some known-good
  ;; values.
  (dolist (name '("python" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

  :diminish flycheck-mode)

(provide 'radian-check)

;;; radian-check.el ends here
