;; -*- lexical-binding: t -*-

(require 'radian-appearance)

(use-feature emacs
  :config

  ;; Don't use tabs for indentation. Use only spaces.
  (setq-default indent-tabs-mode nil)

  (defun radian-reverse-region-characters (beg end)
    "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
    (interactive "*r")
    (insert
     (reverse
      (delete-and-extract-region
       beg end))))

  ;; Trigger auto-fill after punctutation characters, not just
  ;; whitespace.
  (mapcar
   (lambda (c)
     (set-char-table-range auto-fill-chars c t))
   "!-=+]};:'\",.?")

  ;; When filling paragraphs, assume that sentences end with one space
  ;; rather than two.
  (setq sentence-end-double-space nil))

(use-feature simple
  :config

  (define-minor-mode radian-trim-whitespace-mode
    "Minor mode to automatically remove trailing whitespace on save.
If enabled, then saving the buffer deletes all whitespace at the
ends of lines and removes trailing newlines at the end of the
file."
    nil nil nil
    (if radian-trim-whitespace-mode
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local)
      (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

  (define-globalized-minor-mode radian-trim-whitespace-global-mode
    radian-trim-whitespace-mode radian-trim-whitespace-mode)

  (radian-trim-whitespace-global-mode +1)

  (put 'radian-trim-whitespace-mode 'safe-local-variable #'booleanp)

  (defun radian-advice-stop-kill-at-whitespace (kill-line &rest args)
    "Prevent `kill-line' from killing through whitespace to a newline.
This affects the case where you press \\[kill-line] when point is
followed by some whitespace and then a newline. Without this
advice, \\[kill-line] will kill both the whitespace and the
newline, which is inconsistent with its behavior when the
whitespace is replaced with non-whitespace. With this advice,
\\[kill-line] will kill just the whitespace, and another
invocation will kill the newline.

This is an `:around' advice for `kill-line'. KILL-LINE is the
original definition of `kill-line' and ARGS are the arguments
that were passed to it."
    (let ((show-trailing-whitespace t))
      (apply kill-line args)))

  (advice-add #'kill-line :around #'radian-advice-stop-kill-at-whitespace))

(use-feature files
  :config

  ;; When saving a file, insert a trailing newline if one is missing.
  (setq require-final-newline t))

(use-feature text-mode
  :hook ((text-mode . auto-fill-mode)))

(use-feature whitespace-mode
  :init

  (define-minor-mode radian-long-lines-mode
    "Minor mode for highlighting long lines."
    nil nil nil
    (if radian-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)))

  :diminish whitespace-mode)

(provide 'radian-text)
