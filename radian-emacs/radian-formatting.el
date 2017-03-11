;;; radian-formatting.el --- Formatting text

;; Don't use tabs for indentation, even in deeply indented lines.
(setq-default indent-tabs-mode nil)

;; Sentences end with one space, not two. If you end sentences with
;; one space, this has two concrete effects on filling with M-q.
;; Firstly, it allows Emacs to break directly after a sentence.
;; Secondly, consider the case when the end of a line has an
;; end-of-sentence period followed by trailing whitespace and when at
;; least the last word of the sentence is too long for the line. In
;; that case, this option prevents filling from inserting two spaces
;; after the period when it is wrapped to the next line.
(setq sentence-end-double-space nil)

;;; Trim trailing whitespace on save. This will get rid of end-of-line
;;; whitespace, and reduce the number of blank lines at the end of the
;;; file to one.

;; Make a user variable to control whether trailing whitespace is
;; actually removed.
(defvar radian-delete-trailing-whitespace t
  "If non-nil, delete trailing whitespace on save.")

;; Allow setting `radian-delete-trailing-whitespace' from a file-local
;; or directory-local variable list.
(put 'radian-delete-trailing-whitespace
     'safe-local-variable #'booleanp)

;; Make a function to replace `delete-trailing-whitespace' that
;; respects variable `radian-delete-trailing-whitespace'.
(defun radian--maybe-delete-trailing-whitespace ()
  "Maybe delete trailing whitespace in buffer.
Trailing whitespace is only deleted if variable
`radian-delete-trailing-whitespace' if non-nil."
  (when radian-delete-trailing-whitespace
    (delete-trailing-whitespace)))

;; Call `radian--maybe-delete-trailing-whitespace' before saving a
;; file.
(add-hook 'before-save-hook
          #'radian--maybe-delete-trailing-whitespace)

;; Add a trailing newline if there is not one already, when saving.
;; This is enabled for default for certain modes, but we want it
;; everywhere (e.g. when editing .gitignore files).
(setq require-final-newline t)

;; Automatically wrap lines when editing plain text files.
(add-hook 'text-mode-hook #'auto-fill-mode)

(provide 'radian-formatting)

;;; radian-formatting.el ends here
