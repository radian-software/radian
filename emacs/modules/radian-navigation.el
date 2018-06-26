;; -*- lexical-binding: t -*-

(require 'radian-bind-key)
(require 'radian-completion)
(require 'radian-custom)

(use-feature simple
  :config

  (defun radian-advice-allow-negative-pop-mark
      (set-mark-command &optional arg)
    "Allow \\[set-mark-command] to step in reverse.
If a negative prefix argument is given (like
\\[negative-argument] \\[set-mark-command]), then it will step in
the reverse direction from \\[universal-argument]
\\[set-mark-command].

This is an `:around' advice for `set-mark-command'.
SET-MARK-COMMAND is the original function and ARG is its
argument."
    ;; Based on http://stackoverflow.com/a/14539202/3538165.
    (interactive "P")
    (if (< (prefix-numeric-value arg) 0)
        ;; If we don't have any marks set, no-op.
        (when mark-ring
          ;; I can't remember how this code works. Sorry.
          (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
          (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
          (when (null (mark t)) (ding))
          (setq mark-ring (nbutlast mark-ring))
          (goto-char (marker-position (car (last mark-ring)))))
      ;; If no prefix argument, or prefix argument is nonnegative, defer
      ;; to the original behavior.
      (funcall set-mark-command arg)))

  (advice-add #'set-mark-command :around
              #'radian-advice-allow-negative-pop-mark)

  (defun radian--advice-allow-unpopping-global-mark
      (pop-global-mark &optional arg)
    "Allow \\[pop-global-mark] to step in reverse.
If a negative prefix argument is given (like
\\[negative-argument] \\[pop-global-mark]), then it will step in
the reverse direction from \\[pop-global-mark].

This is an `:around' advice for `pop-global-mark'.
POP-GLOBAL-MARK is the original function and ARG is its
argument."
    (interactive "P")
    (if arg
        ;; Tweaked from the implementation of `pop-global-mark'.
        (progn
          (or global-mark-ring
              (error "No global mark set"))
          ;; We need to do this earlier than `pop-global-mark' does the
          ;; corresponding action in order to properly undo its
          ;; behavior.
          (setq global-mark-ring (nconc (list (car (last global-mark-ring)))
                                        (butlast global-mark-ring)))
          (while (and global-mark-ring (not (marker-buffer (car (last global-mark-ring)))))
            (setq global-mark-ring (butlast global-mark-ring)))
          (let* ((marker (car (last global-mark-ring)))
                 (buffer (marker-buffer marker))
                 (position (marker-position marker)))
            (set-buffer buffer)
            (or (and (>= position (point-min))
                     (<= position (point-max)))
                (if widen-automatically
                    (widen)
                  (error "Global mark position is outside accessible part of buffer")))
            (goto-char position)
            (switch-to-buffer buffer)))
      (funcall pop-global-mark)))

  (advice-add #'pop-global-mark :around
              #'radian--advice-allow-unpopping-global-mark))

(use-feature isearch
  :config

  ;; Eliminate the quarter-second delay before I-search matches are
  ;; highlighted, because delays suck.
  (setq lazy-highlight-initial-delay 0))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs))

;; Package `swiper' provides an alternative to `isearch' which instead
;; uses `ivy' to display and select from the results.
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Package `avy' provides a quick-navigation mechanism wherein you
;; enter some query, like the first letter of a word, and each place
;; that query matches on-screen is assigned a sequence of keystrokes.
;; Pressing those keystrokes moves point there.
(use-package avy
  :init

  (radian-bind-key "l" #'avy-goto-line)
  (radian-bind-key "w" #'avy-goto-word-1)
  (radian-bind-key "c" #'avy-goto-char))

(use-feature bookmark
  :config

  (defalias 'radian-advice-bookmark-silence #'ignore
    "Silence useless messages from bookmark.el.
This is an `:override' advice for `bookmark-maybe-message'.")

  (advice-add #'bookmark-maybe-message :override
              #'radian-advice-bookmark-silence))

(use-feature xref
  :config

  ;; When using M-. and friends, always prompt for the identifier (it
  ;; defaults to the identifier at point). This behavior is more
  ;; consistent and predictable than the default, which is to jump
  ;; immediately if there is a valid symbol at point.
  (setq xref-prompt-for-identifier t))

(provide 'radian-navigation)
