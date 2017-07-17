;;; radian-snippet.el --- Configuring snippet tools

(require 'radian-appearance)
(require 'radian-package)

;; Don't show `abbrev-mode' in the mode line.
(with-eval-after-load 'abbrev
  (diminish 'abbrev-mode))

;; YASnippet allows the expansion of user-defined abbreviations into
;; fillable templates. It is also used by `clj-refactor' for some of
;; its refactorings.
(use-package yasnippet
  :demand t
  :config

  ;; Reduce verbosity. The default value is 3. Bumping it down to 2
  ;; eliminates a message about successful snippet lazy-loading setup
  ;; on every(!) Emacs init. Errors should still be shown.
  (setq yas-verbosity 2)

  ;; Enable YASnippet everywhere.
  (yas-global-mode)

  :diminish yas-minor-mode)

(provide 'radian-snippet)

;;; radian-snippet.el ends here
