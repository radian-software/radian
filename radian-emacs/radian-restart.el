;;; radian-restart.el --- Restarting Emacs

(require 'radian-package)

;; This package provides an easy way to restart Emacs from within
;; Emacs, both in the terminal and in windowed mode. I picked the
;; binding C-x M-c because it parallels C-x C-c to exit Emacs.
(use-package restart-emacs
  :bind ("C-x M-c" . restart-emacs))

(provide 'radian-restart)

;;; radian-restart.el ends here
