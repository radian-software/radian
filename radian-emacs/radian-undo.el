;;; radian-undo.el --- Undo and redo

(require 'radian-package)

;; This package provides undo/redo commands that are both more
;; intuitive and more powerful than the Emacs defaults. It also allows
;; you to visualize the undo/redo tree, which uses a branching model
;; to ensure that you can never lose changes.
(use-package undo-tree
  :demand t
  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is bound
         ;; to C-_ and C-/, and `undo-tree-redo' is bound to M-_. It's
         ;; logical to also bind M-/ to `undo-tree-redo'. This overrides the
         ;; default binding of M-/, which is to `dabbrev-expand'.
         :map undo-tree-map
         ("M-/" . undo-tree-redo))
  :config

  ;; Enable Undo Tree everywhere.
  (global-undo-tree-mode 1)

  ;; Don't show Undo Tree in the mode line.
  (setq undo-tree-mode-lighter nil)

  ;; Suppress the message saying that the undo history file was
  ;; saved (because this happens every single time you save a file).

  (defun radian--advice-suppress-undo-history-saved-message
      (undo-tree-save-history &rest args)
    "Suppress the annoying message saying undo history was saved.
This is an `:around' advice for `undo-tree-save-history'. It's
not actually necessary with the default configuration, but is
important if you enable `undo-tree-auto-save-history'."
    (let ((inhibit-message t))
      (apply undo-tree-save-history args)))

  (advice-add #'undo-tree-save-history :around
              #'radian--advice-suppress-undo-history-saved-message)

  ;; Suppress the message saying that the undo history could not be
  ;; loaded because the file changed outside of Emacs.

  (defun radian--advice-suppress-undo-tree-buffer-modified-message
      (undo-tree-load-history &rest args)
    "Suppress the annoying message saying undo history could not be loaded.
This is an `:around' advice for `undo-tree-load-history', and
suppresses the message you get when undo history could not be
loaded since the file was changed outside of Emacs."
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  (advice-add #'undo-tree-load-history :around
              #'radian--advice-suppress-undo-tree-buffer-modified-message)

  ;; Disable undo-in-region. It sounds like an interesting feature,
  ;; but unfortunately the implementation is very buggy and regularly
  ;; causes you to lose your undo history.
  (setq undo-tree-enable-undo-in-region nil))

(provide 'radian-undo)

;;; radian-undo.el ends here
