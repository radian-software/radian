;;; radian-check.el --- On-the-fly syntax and semantics checking

(require 'radian-package)

(use-package flycheck
  :init

  ;; Enable Flycheck everywhere.
  (global-flycheck-mode)

  :config

  ;; Disable Flycheck for Emacs Lisp (it's dangerous to byte-compile
  ;; arbitrary work-in-progress code!).
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))

  :diminish flycheck-mode)

(provide 'radian-check)

;;; radian-check.el ends here
