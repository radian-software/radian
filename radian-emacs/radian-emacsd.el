;;; radian-emacsd.el --- Organizing ~/.emacs.d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Organizing ~/.emacs.d

(require 'radian-package)

;; This package changes the default paths for lots of different
;; packages, with the net result that the ~/.emacs.d folder is much
;; more clean and organized. See the README [1]. We're using my fork
;; until a pull request for `historian' is merged [2].
;;
;; [1]: https://github.com/raxod502/no-littering
;; [2]: https://github.com/tarsius/no-littering/pull/45
(use-package no-littering
  :recipe (:fetcher github
           :repo "raxod502/no-littering")
  :demand t)

(provide 'radian-emacsd)

;;; radian-emacsd.el ends here
