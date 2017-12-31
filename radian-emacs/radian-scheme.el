;;; radian-scheme.el --- Support for Schemes

(require 'radian-autocomplete)
(require 'radian-indent)
(require 'radian-lisp)
(require 'radian-package)

(use-package scheme
  :straight nil
  :config

  (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

;; Provides Racket REPL integration, including documentation and
;; source lookups. Basically CIDER for Racket.
(use-package geiser)

(provide 'radian-scheme)

;;; radian-scheme.el ends here
