;;; radian-check.el --- On-the-fly syntax and semantics checking

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting, or more generally syntax checking. It comes
;; with a large number of checkers pre-defined, and other packages
;; define more.
(use-package flycheck
  :straight (:host github :repo "raxod502/flycheck" :branch "fork/1"
                   :upstream (:host github :repo "flycheck/flycheck"
                                    :branch "master"))
  :init

  :defer 3
  :config

  ;; Enable `flycheck' everywhere unless otherwise specified.
  (global-flycheck-mode +1)

  ;; Make `flycheck-python-pycompile-executable' safe to set in a
  ;; file-local variable.
  (dolist (name '("python" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one. (This has
  ;; no effect until [1] is merged.)
  ;;
  ;; [1]: https://github.com/flycheck/flycheck/pull/1308
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch)

  ;; Allow disabling Flycheck in a buffer-local or file-local variable.
  (put 'flycheck-mode 'safe-local-variable #'booleanp)

  ;; Disable Flycheck's mode-line indicator.
  (setq flycheck-mode-line nil))

(provide 'radian-check)

;;; radian-check.el ends here
