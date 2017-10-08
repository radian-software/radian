;;; radian-indent.el --- Indentation

(require 'radian-package)

(define-minor-mode radian-slow-indent-mode
  "Minor mode for when the indentation code is slow.
This prevents `aggressive-indent' from indenting as frequently.")

;; This package keeps indentation correct at all times. It can be a
;; phenomenal productivity booster, but only if your indentation is
;; consistent.
(use-package aggressive-indent
  :init

  ;; Allow disabling/enabling `aggressive-indent-mode' from a
  ;; file-local or directory-local variable list.
  (put 'aggressive-indent-mode 'safe-local-variable #'booleanp)

  :config

  ;; Register `aggressive-indent' in `radian-slow-indent-mode'.

  (defun radian-aggressive-indent-toggle-slow ()
    "Slow down `aggressive-indent' by disabling reindentation on save.
This is done in `radian-slow-indent-mode'."
    ;; If `aggressive-indent' hasn't been enabled yet, we might have
    ;; to wait to do the following code, so defer it using the mode
    ;; hook. Running this function is supposed to be idempotent and
    ;; generally safe, so we can do it whenever.
    (add-hook 'aggressive-indent-mode-hook
              #'radian-aggressive-indent-toggle-slow)
    (if (or radian-slow-indent-mode (not aggressive-indent-mode))
        (remove-hook 'before-save-hook
                     ;; Yes, this is a typo in `aggressive-indent'.
                     #'aggressive-indent--proccess-changed-list-and-indent
                     'local)
      (add-hook 'before-save-hook
                #'aggressive-indent--proccess-changed-list-and-indent
                nil 'local)))

  (add-hook 'radian-slow-indent-mode #'radian-aggressive-indent-toggle-slow)

  :diminish "AggrIndent")

(provide 'radian-indent)

;;; radian-indent.el ends here
