;;; radian-restart.el --- Restarting Emacs

(require 'radian-bind-key)
(require 'radian-package)
(require 'radian-patch)

;; This package provides an easy way to restart Emacs from within
;; Emacs, both in the terminal and in windowed mode. I picked the
;; binding C-x M-c because it parallels C-x C-c to exit Emacs.
(use-package restart-emacs
  :init

  (el-patch-feature restart-emacs)

  :bind (("C-x M-c" . restart-emacs)
         ("C-x   C" . radian-new-emacs))
  :config

  (el-patch-defun (el-patch-swap restart-emacs radian-new-emacs)
    (&optional args)
    "Restart Emacs.

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is restarted
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is restarted with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be restarted."
    (interactive "P")
    ;; Do not trigger a restart unless we are sure, we can restart emacs
    (restart-emacs--ensure-can-restart)
    ;; We need the new emacs to be spawned after all kill-emacs-hooks
    ;; have been processed and there is nothing interesting left
    (let* ((default-directory (restart-emacs--guess-startup-directory))
           (translated-args (if (called-interactively-p 'any)
                                (restart-emacs--translate-prefix-to-args args)
                              args))
           (restart-args (append translated-args
                                 ;; When Emacs is started with a -Q
                                 ;; restart-emacs's autoloads would not be present
                                 ;; causing the the --restart-emacs-desktop
                                 ;; argument to be unhandled
                                 (unless (member "-Q" translated-args)
                                   (restart-emacs--frame-restore-args))))
           (el-patch-remove
             (kill-emacs-hook (append kill-emacs-hook
                                      (list (apply-partially #'restart-emacs--launch-other-emacs
                                                             restart-args))))))
      (el-patch-swap
        (save-buffers-kill-emacs)
        (restart-emacs--launch-other-emacs restart-args)))))

(provide 'radian-restart)

;;; radian-restart.el ends here
