;;; radian-elisp.el --- Support for Emacs Lisp

(require 'radian-bind-key)
(require 'radian-check)
(require 'radian-custom)
(require 'radian-eldoc)
(require 'radian-indent)
(require 'radian-lisp)
(require 'radian-patch)

;; Enable ElDoc for Elisp buffers and the *scratch* buffer.
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; Enable Aggressive Indent for Elisp buffers and the *scratch*
;; buffer.
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

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
      (load buffer-file-name nil 'nomessage)))
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
