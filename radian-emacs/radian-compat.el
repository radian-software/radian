;;; radian-compat.el --- Compatibility for older Emacs versions

(require 'subr-x)

;; Define `if-let*' and `when-let*' for Emacs versions older than 26.1.

(unless (fboundp 'if-let*)
  (defalias 'if-let* #'if-let))

(unless (fboundp 'when-let*)
  (defalias 'when-let* #'when-let))

(provide 'radian-compat)

;;; radian-compat.el ends here
