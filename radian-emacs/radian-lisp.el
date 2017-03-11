;;; radian-lisp.el --- Support for Lisps

(require 'radian-indent)
(require 'radian-package)

;; This is a very useful package that keeps parentheses balanced at
;; all times, and provides structural navigation and editing commands
;; for s-expressions.
(use-package paredit)

;; Enable Paredit when editing Lisps other than Emacs Lisp. See also
;; `radian-elisp', though.
(add-hook 'lisp-mode #'paredit-mode)

;; Enable Aggressive Indent when editing Lisps other than Emacs Lisp.
(add-hook 'lisp-mode #'aggressive-indent-mode)

(provide 'radian-lisp)

;;; radian-lisp.el ends here
