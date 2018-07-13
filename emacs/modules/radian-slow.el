;; -*- lexical-binding: t -*-

(define-minor-mode radian-slow-autocomplete-mode
  "Minor mode for when the autocompletion code is slow.
This prevents `company' and `eldoc' from displaying metadata as
quickly.")

(provide 'radian-slow)
