;;; radian-formatting.el --- Formatting text

(require 'radian-appearance)
(require 'radian-package)

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

;; Use an adaptive fill prefix when visually wrapping too-long lines.
;; This means that if you have a line that is long enough to wrap
;; around, the prefix (e.g. comment characters or indent) will be
;; displayed again on the next visual line.
(use-package filladapt
  :demand t
  :diminish filladapt-mode
  :config

  ;; Enable filladapt (almost) everywhere.

  (define-globalized-minor-mode global-filladapt-mode
    filladapt-mode
    (lambda ()
      ;; If we enable `filladapt-mode' in `clojure-mode', then it
      ;; breaks `fill-paragraph' in docstrings (Emacs can no longer
      ;; tell when the docstring ends and the code begins).
      (unless (derived-mode-p 'clojure-mode)
        (filladapt-mode))))

  (global-filladapt-mode 1))

;; Make `fill-paragraph' generally smarter. For example, it now
;; behaves nicely in Markdown's bulleted lists.
(use-package adaptive-wrap
  :demand t
  :config

  ;; Enable adaptive-wrap everywhere.

  (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
    adaptive-wrap-prefix-mode adaptive-wrap-prefix-mode)

  (global-adaptive-wrap-prefix-mode))

;; Useful utility function. Like `reverse-region', but works
;; characterwise rather than linewise.
(defun radian-reverse-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

;; Don't show `whitespace-mode' in the mode line.
(diminish 'whitespace-mode)

;; Minor mode for configuring `whitespace-mode' to highlight long
;; lines.
(define-minor-mode radian-long-lines-mode
  "When enabled, highlight long lines."
  nil nil nil
  (if radian-long-lines-mode
      (progn
        (setq-local whitespace-style '(face lines-tail))
        (whitespace-mode 1))
    (whitespace-mode -1)
    (kill-local-variable 'whitespace-style)))

;; Insert paired delimiters even when Paredit is not active.
(electric-pair-mode 1)

;; Be slightly more conservative with regard to inserting paired
;; delimiters.
(setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)

;; Support for EditorConfig, a "file format and collection of text
;; editor plugins for maintaining consistent coding styles between
;; different editors and IDEs".
(use-package editorconfig
  :defer-install t
  :mode ("/\\.editorconfig\\'" . editorconfig-conf-mode))

(provide 'radian-formatting)

;;; radian-formatting.el ends here
