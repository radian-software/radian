;;; radian-help.el --- Improve the Emacs help system

(require 'radian-package)

;; This package provides several useful features, but the most
;; important is `describe-keymap'. Because the package is rather
;; outdated, it's not autoloaded. (But `use-package' takes care of
;; that for us.)
(use-package help-fns+
  :defer-install t
  :bind (("C-h M-k" . describe-keymap)
         ;; Prevent help-fns+ from overriding this built-in
         ;; keybinding:
         ("C-h o" . describe-symbol)))

(provide 'radian-help)

;;; radian-help.el ends here
