;;; radian-bind-key.el --- Utility functions for keybindings

(require 'radian-custom)

(defcustom radian-prefix "M-P"
  "Prefix key sequence for Radian-related keybindings.
This is a string as would be passed to `kbd'."
  :group 'radian
  :type 'string)

;; This function is used in e.g. `radian-register-dotfile'.
(defun radian-join-keys (&rest keys)
  "Join key sequences. Empty strings and nils are discarded.
\(radian--join-keys \"M-P e\" \"e i\") => \"M-P e e i\"
\(radian--join-keys \"M-P\" \"\" \"e i\") => \"M-P e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

;; Package `bind-key' provides a macro by the same name, as well as a
;; few others, that provides a much prettier API than `define-key' and
;; `global-set-key' do. It's also the same API that the `:bind' and
;; similar keywords in `use-package' use.
(use-package bind-key)

(provide 'radian-bind-key)

;;; radian-bind-key.el ends here
