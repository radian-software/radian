;;; radian-slow.el --- For when Emacs just isn't fast enough

(define-minor-mode radian-slow-indent-mode
  "Minor mode for when the indentation code is slow.
This prevents `aggressive-indent' from indenting as frequently.")

(define-minor-mode radian-slow-autocomplete-mode
  "Minor mode for when the autocompletion code is slow.
This prevents `company' and `eldoc' from displaying metadata as
quickly.")

(provide 'radian-slow)

;;; radian-slow.el ends here
