;;; radian-check.el --- On-the-fly syntax and semantics checking

(require 'radian-package)

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting, or more generally syntax checking. It comes
;; with a large number of checkers pre-defined, and other packages
;; define more.
(use-package flycheck
  ;; My fork adds support for running a syntax check when changing
  ;; buffers. See [1].
  ;;
  ;; [1]: https://github.com/flycheck/flycheck/pull/1308
  :recipe (:host github :repo "raxod502/flycheck"
           :upstream (:host github :repo "flycheck/flycheck"))
  :init

  ;; Enable `flycheck' everywhere unless otherwise specified.
  (global-flycheck-mode +1)

  :config

  ;; Make `flycheck-python-pycompile-executable' safe to set in a
  ;; file-local variable.
  (dolist (name '("python" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one.
  (add-to-list 'flycheck-check-syntax-automatically 'buffer-switch)

  :diminish flycheck-mode)

(provide 'radian-check)

;;; radian-check.el ends here
