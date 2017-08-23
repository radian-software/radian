;;; radian-lisp.el --- Support for Lisps

(require 'radian-indent)

;; Enable Aggressive Indent when editing Lisps other than Emacs Lisp.
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

(provide 'radian-lisp)

;;; radian-lisp.el ends here
