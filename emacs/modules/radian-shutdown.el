;; -*- lexical-binding: t -*-

(require 'radian-bind-key)
(require 'radian-patch)

;; Package `restart-emacs' provides an easy way to restart Emacs from
;; inside of Emacs, both in the terminal and in windowed mode.
(use-package restart-emacs
  :commands (radian-new-emacs)
  :init

  (defvar radian-shutdown-restart-in-progress nil
    "Used to prevent infinite recursion.
This is non-nil if `radian-advice-kill-emacs-dispatch' has called
`restart-emacs'.")

  (cl-defun radian-advice-kill-emacs-dispatch
      (save-buffers-kill-emacs &optional arg)
    "Allow restarting Emacs or starting a new session on shutdown.

This is an `:around' advice for `save-buffers-kill-emacs'.
SAVE-BUFFERS-KILL-EMACS is the original function and ARG is its
argument."
    (if radian-shutdown-restart-in-progress
        (funcall save-buffers-kill-emacs arg)
      (let ((prompt "Really exit (or restart, or start new) Emacs? (y/n/r/e) ")
            (key nil))
        (while (null key)
          (let ((cursor-in-echo-area t))
            (when minibuffer-auto-raise
              (raise-frame (window-frame (minibuffer-window))))
            (pcase (setq key
                         (read-key (propertize prompt
                                               'face 'minibuffer-prompt)))
              ((or ?y ?Y) (funcall save-buffers-kill-emacs arg))
              ((or ?n ?N))
              ((or ?r ?R) (let ((radian-shutdown-restart-in-progress t))
                            (restart-emacs arg)))
              ((or ?e ?E) (radian-new-emacs arg))
              (?\C-g (signal 'quit nil))
              (_ (setq key nil)))))
        (message "%s%c" prompt key))))

  (advice-add #'save-buffers-kill-emacs :around
              #'radian-advice-kill-emacs-dispatch)

  :config/el-patch

  (defun (el-patch-swap restart-emacs radian-new-emacs)
      (&optional args)
    (el-patch-concat
      (el-patch-swap
        "Restart Emacs."
        "Start a new Emacs session without killing the current one.")
      "

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is "
      (el-patch-swap "restarted" "started")
      "
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is "
      (el-patch-swap "restarted" "started")
      " with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be "
      (el-patch-swap "restarted" "started")
      ".")
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

(provide 'radian-shutdown)
