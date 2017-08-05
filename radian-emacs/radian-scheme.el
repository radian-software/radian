;;; radian-scheme.el --- Support for Schemes

(require 'radian-autocomplete)
(require 'radian-indent)
(require 'radian-lisp)
(require 'radian-package)

(with-eval-after-load 'scheme

  ;; Enable Aggressive Indent for Scheme.
  (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

;; Provides Racket REPL integration, including documentation and
;; source lookups. Basically CIDER for Racket.
(use-package geiser
  :defer-install t
  :commands (run-chez
             run-chibi
             run-chicken
             run-geiser
             run-guile
             run-mit
             run-racket))

(provide 'radian-scheme)

;;; radian-scheme.el ends here
