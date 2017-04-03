;;; radian-bind-key.el --- Utility functions for keybindings

(require 'radian-custom)
(require 'radian-package)

(defcustom radian-prefix "M-P"
  "Prefix key sequence for Radian-related keybindings."
  :group 'radian
  :type 'string)

;; This function is used in e.g. `radian-register-dotfile'.
(defun radian-join-keys (&rest keys)
  "Join key sequences. Empty strings and nils are discarded.
\(radian--join-keys \"M-P e\" \"e i\") => \"M-P e e i\"
\(radian--join-keys \"M-P\" \"\" \"e i\") => \"M-P e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

;; This library provides keybinding functions that are nicer to use
;; than the ones that come with Emacs. This whole file is not strictly
;; necessary, since `bind-key' is actually a dependency of
;; `use-package' and is therefore already loaded by time we get here,
;; but I think it is less confusing for files that use `bind-key' to
;; require `radian-bind-key' than to require `radian-package'. Note
;; that `bind-key' is versioned in the same repository as
;; `use-package'.
(use-package bind-key)

(provide 'radian-bind-key)

;;; radian-bind-key.el ends here
