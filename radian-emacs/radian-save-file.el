;;; radian-save-file.el --- Saving files

;; Always use copying to make backup files. This prevents hard links
;; from being made to point at the backup file rather than the
;; original.
(setq backup-by-copying t)

;; Keep multiple numbered backup files, rather than a single
;; unnumbered backup file.
(setq version-control t)

;; Delete old backups silently, instead of asking for confirmation.
(setq delete-old-versions t)

;; Don't make autosave files.
(setq auto-save-default nil)

(provide 'radian-save-file)

;;; radian-save-file.el ends here
