;;; radian-help.el --- Improve the Emacs help system

(require 'radian-package)

;; Suppress the "Type q in help window to delete it" hints.

(defalias 'radian--advice-inhibit-help-window-hints #'ignore
  "Inhibit the \"Type q in help window to delete it\" hints.
This is an `:override' advice for
`help-window-display-message'.")

(advice-add #'help-window-display-message :override
            #'radian--advice-inhibit-help-window-hints)

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

;; Don't show the search field in Custom.
(setq custom-search-field nil)

(provide 'radian-help)

;;; radian-help.el ends here
