;; -*- lexical-binding: t -*-

(require 'radian-bind-key)
(require 'radian-os)
(require 'radian-patch)
(require 'radian-revert)

;; For some reason, the autoloads from `dired-aux' and `dired-x' are
;; not loaded automatically. Do it.
(require 'dired-loaddefs)

(use-feature files
  :config

  ;; Dired has some trouble parsing out filenames that have e.g.
  ;; leading spaces, unless the ls program used has support for Dired.
  ;; GNU ls has this support, so if it is available we tell Dired to
  ;; use it.
  (when (executable-find "gls")
    (setq insert-directory-program "gls")))

(use-feature dired
  :config

  ;; Prevent Dired from printing a message if your ls does not support
  ;; the --dired option. (We do this by performing the check
  ;; ourselves, and refraining from printing a message in the
  ;; problematic case.)

  (defun radian-advice-dired-check-for-ls-dired (&rest _)
    "Check if ls --dired is supported ahead of time, and silently.
This is a `:before' advice for `dired-insert-directory'."
    (when (eq dired-use-ls-dired 'unspecified)
      (setq dired-use-ls-dired
            (eq 0 (call-process insert-directory-program
                                nil nil nil "--dired")))))

  (advice-add #'dired-insert-directory :before
              #'radian-advice-dired-check-for-ls-dired)

  ;; Prevent `auto-revert-mode' from showing messages in the echo area
  ;; in Dired mode.
  (add-hook 'dired-mode-hook #'radian-revert-silence)

  ;; Disable the prompt about whether I want to kill the Dired buffer
  ;; for a deleted directory. Of course I do! It's just a Dired
  ;; buffer, after all. Note that this variable, for reasons unknown
  ;; to me, is defined in `dired-x', but only affects the behavior of
  ;; functions defined in `dired'.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Instantly revert Dired buffers on re-visiting them, with no
  ;; message. (A message is shown if insta-revert is either disabled or
  ;; determined dynamically by setting this variable to a function.)
  (setq dired-auto-revert-buffer t))

(use-feature dired-x
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil))

(provide 'radian-dired)
