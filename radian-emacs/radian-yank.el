;;; radian-yank.el --- Killing and yanking

;; If you start typing when you have something selected, then the
;; selection will be deleted. If you press DEL while you have
;; something selected, it will be deleted rather than
;; killed. (Otherwise, in both cases the selection is deselected and
;; the normal function of the key is performed.)
(delete-selection-mode 1)

;; Make delete-selection-mode work properly with AUCTeX.
(with-eval-after-load 'latex
  (put 'LaTeX-insert-left-brace 'delete-selection t))

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

(provide 'radian-yank)

;;; radian-yank.el ends here
