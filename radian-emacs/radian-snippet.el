;;; radian-snippet.el --- Configuring snippet tools

(require 'radian-appearance)
(require 'radian-package)

;; Don't show `abbrev-mode' in the mode line.
(with-eval-after-load 'abbrev-mode
  (diminish 'abbrev-mode))

;; YASnippet allows the expansion of user-defined abbreviations into
;; fillable templates. It is also used by `clj-refactor' for some of
;; its refactorings.
(use-package yasnippet
  :diminish yas-minor-mode)

(provide 'radian-snippet)

;;; radian-snippet.el ends here
