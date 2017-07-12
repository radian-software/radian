;;; radian-emacsd.el --- Organizing ~/.emacs.d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Organizing ~/.emacs.d

(require 'radian-package)

;; This package changes the default paths for lots of different
;; packages, with the net result that the ~/.emacs.d folder is much
;; more clean and organized. See the README [1].
;;
;; [1]: https://github.com/raxod502/no-littering
(use-package no-littering
  :demand t)

(provide 'radian-emacsd)

;;; radian-emacsd.el ends here
