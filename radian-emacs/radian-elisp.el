;;; radian-elisp.el --- Support for Emacs Lisp

(require 'radian-bind-key)
(require 'radian-check)
(require 'radian-custom)
(require 'radian-eldoc)
(require 'radian-indent)
(require 'radian-lisp)
(require 'radian-package)
(require 'radian-patch)

;; Enable ElDoc for Elisp buffers and the *scratch* buffer.
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; Enable Aggressive Indent for Elisp buffers and the *scratch*
;; buffer.
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; Fix the indentation of keyword lists in Emacs Lisp. See [1] and [2].
;;
;; Before:
;;  (:foo bar
;;        :baz quux)
;;
;; After:
;;  (:foo bar
;;   :bar quux)
;;
;; [1]: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
;; [2]: http://emacs.stackexchange.com/q/10230/12534
(el-patch-defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (el-patch-let (($cond (and (elt state 2)
                             (el-patch-wrap 1 1
                               (or (not (looking-at "\\sw\\|\\s_"))
                                   (looking-at ":")))))
                 ($then (progn
                          (if (not (> (save-excursion (forward-line 1) (point))
                                      calculate-lisp-indent-last-sexp))
                              (progn (goto-char calculate-lisp-indent-last-sexp)
                                     (beginning-of-line)
                                     (parse-partial-sexp (point)
                                                         calculate-lisp-indent-last-sexp 0 t)))
                          ;; Indent under the list or under the first sexp on the same
                          ;; line as calculate-lisp-indent-last-sexp.  Note that first
                          ;; thing on that line has to be complete sexp since we are
                          ;; inside the innermost containing sexp.
                          (backward-prefix-chars)
                          (current-column)))
                 ($else (let ((function (buffer-substring (point)
                                                          (progn (forward-sexp 1) (point))))
                              method)
                          (setq method (or (function-get (intern-soft function)
                                                         'lisp-indent-function)
                                           (get (intern-soft function) 'lisp-indent-hook)))
                          (cond ((or (eq method 'defun)
                                     (and (null method)
                                          (> (length function) 3)
                                          (string-match "\\`def" function)))
                                 (lisp-indent-defform state indent-point))
                                ((integerp method)
                                 (lisp-indent-specform method state
                                                       indent-point normal-indent))
                                (method
                                 (funcall method indent-point state))))))
    (let ((normal-indent (current-column))
          (el-patch-add
            (orig-point (point))))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (el-patch-swap
        (if $cond
            ;; car of form doesn't seem to be a symbol
            $then
          $else)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ($cond $then)
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t $else))))))

(defcustom radian-reload-init-keybinding
  (radian-join-keys radian-prefix "r")
  "The keybinding for reloading init.el, as a string.
Nil means no keybinding is established."
  :group 'radian
  :type 'string)

;; Add a keybinding for reloading init.el. This is useful for when you
;; have several instances of Emacs open and you change something in
;; your configuration, then later come back to an old Emacs that was
;; opened before you made the change. You can then just press the
;; keybinding to get the change into that instance.

(defun radian-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(bind-key radian-reload-init-keybinding #'radian-reload-init)

;; Add a keybinding (C-c C-k) for evaluating a buffer of Elisp. This
;; is consistent with the keybindings for evaluating a buffer in CIDER
;; and Geiser.

(defun radian-eval-buffer ()
  "Evaluate the current buffer as Elisp code."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load-file buffer-file-name)))
  (message "Evaluating %s... done." (buffer-name)))

(bind-key "C-c C-k" #'radian-eval-buffer emacs-lisp-mode-map)

;; Add keybindings (C-h C-f and C-h C-v) for jumping to the source of
;; Elisp functions and variables. Also, add a keybinding (C-h C-o)
;; that performs the functionality of M-. only for Elisp, because the
;; latter command is often rebound by other major modes. Note that
;; this overrides the default bindings of C-h C-f to `view-emacs-FAQ'
;; and C-h C-o to `describe-distribution', but I think this is not
;; very important.

(defun find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend)))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

(bind-keys
 ("C-h C-f" . find-function)
 ("C-h C-v" . find-variable)
 ("C-h C-o" . find-symbol))

;; Show `lisp-interaction-mode' as "Lisp-Interaction" instead of "Lisp
;; Interaction" in the mode line.

(defun radian--rename-lisp-interaction-mode ()
  (setq mode-name "Lisp-Interaction"))

(add-hook 'lisp-interaction-mode-hook
          #'radian--rename-lisp-interaction-mode)

;; Disable the checkdoc Flycheck checker for Emacs Lisp, as it appears
;; to not work in some situations.
(with-eval-after-load 'flycheck
  (defun radian--disable-flycheck-for-emacs-lisp ()
    "Disable Flycheck checkers for Elisp."
    (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

  (add-hook 'emacs-lisp-mode-hook #'radian--disable-flycheck-for-emacs-lisp))

(provide 'radian-elisp)

;;; radian-elisp.el ends here
