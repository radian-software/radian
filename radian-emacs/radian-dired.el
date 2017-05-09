;;; radian-dired.el --- Filesystem management via Dired

(require 'radian-bind-key)
(require 'radian-os)

;; Make C-x C-j jump to the current file in Dired. For some reason,
;; the autoload for `dired-jump' does not appear to work correctly, so
;; we have to autoload the functions explicitly.

(autoload #'dired-jump "dired-x")
(autoload #'dired-jump-other-window "dired-x")

(bind-keys
 ("C-x C-j" . dired-jump)
 ("C-x 4 C-j" . dired-jump-other-window))

;; Dired has some trouble parsing out filenames that have e.g. leading
;; spaces, unless the ls program used has support for Dired. GNU ls
;; has this support, so if it is available we tell Dired to use it.
;; Otherwise, we tell Dired to not attempt to use the --dired option
;; and to just do its best.
(radian-with-operating-system macOS
  (if (executable-find "gls")
      (setq insert-directory-program "gls")
    (setq dired-use-ls-dired nil)))

;; Prevent `auto-revert-mode' from showing messages in the echo area
;; in Dired mode.

(defun radian--silence-auto-revert-mode ()
  "Silence `auto-revert-mode' in the current buffer.
This function is to be placed on `dired-mode-hook'."
  (setq-local auto-revert-verbose nil))

(add-hook 'dired-mode-hook #'radian--silence-auto-revert-mode)

(provide 'radian-dired)

;;; radian-dired.el ends here
