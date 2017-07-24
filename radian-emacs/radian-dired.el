;;; radian-dired.el --- Filesystem management via Dired

(require 'radian-bind-key)
(require 'radian-os)
(require 'radian-patch)

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

(el-patch-feature dired nil)

(with-eval-after-load 'dired

  ;; Skip the silly prompt about whether I want to kill the Dired
  ;; buffer for a deleted directory. Of course I do! It's just a Dired
  ;; buffer, after all.
  (el-patch-defun dired-clean-up-after-deletion (fn)
    "Clean up after a deleted file or directory FN.
Removes any expanded subdirectory of deleted directory.
If `dired-x' is loaded and `dired-clean-up-buffers-too' is non-nil,
also offers to kill buffers visiting deleted files and directories."
    (save-excursion (and (cdr dired-subdir-alist)
                         (dired-goto-subdir fn)
                         (dired-kill-subdir)))
    ;; Offer to kill buffer of deleted file FN.
    (when (and (featurep 'dired-x) dired-clean-up-buffers-too)
      (let ((buf (get-file-buffer fn)))
        (and buf
             (funcall #'y-or-n-p
                      (format "Kill buffer of %s, too? "
                              (file-name-nondirectory fn)))
             (kill-buffer buf)))
      (let ((buf-list (dired-buffers-for-dir (expand-file-name fn))))
        (and buf-list
             (el-patch-remove
               (y-or-n-p (format "Kill Dired buffer%s of %s, too? "
                                 (dired-plural-s (length buf-list))
                                 (file-name-nondirectory fn))))
             (dolist (buf buf-list)
               (kill-buffer buf)))))))

(provide 'radian-dired)

;;; radian-dired.el ends here
