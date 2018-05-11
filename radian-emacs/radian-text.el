;;; radian-text.el --- Formatting and editing text

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
       beg end)))))

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

  (put 'radian-trim-whitespace-mode 'safe-local-variable #'booleanp))

(use-feature paragraphs
  :config

  ;; When filling paragraphs, assume that sentences end with one space
  ;; rather than two.
  (setq sentence-end-double-space nil))

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

;;; radian-text.el ends here
