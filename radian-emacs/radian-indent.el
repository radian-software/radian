;;; radian-indent.el --- Indentation

(require 'radian-package)

;; This package keeps indentation correct at all times. It can be a
;; phenomenal productivity booster, but only if your indentation is
;; consistent.
(use-package aggressive-indent
  :init

  ;; Allow disabling/enabling `aggressive-indent-mode' from a
  ;; file-local or directory-local variable list.
  (put 'aggressive-indent-mode 'safe-local-variable #'booleanp)

  :diminish (aggressive-indent-mode . "AggrIndent"))

(provide 'radian-indent)

;;; radian-indent.el ends here
