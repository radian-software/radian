;;; radian-eldoc.el --- Eldoc customizations

(require 'radian-slow)

;; Disable ElDoc globally. This prevents it from being enabled in the
;; minibuffer (ElDoc messages go into the mode line when you are in
;; the minibuffer, which looks very bad with Radian's default color
;; scheme).
(global-eldoc-mode -1)

;; Show ElDoc messages in the echo area immediately, instead of after
;; 1/2 a second.
(setq eldoc-idle-delay 0)

;; Always truncate ElDoc messages to one line. This prevents the echo
;; area from resizing itself unexpectedly when point is on a variable
;; with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;; Don't show ElDoc in the mode line.
(setq eldoc-minor-mode-string nil)

(defun radian-eldoc-toggle-slow ()
  "Slow down `eldoc' by turning up the delay before metadata is shown.
This is done in `radian-slow-autocomplete-mode'."
  (if radian-slow-autocomplete-mode
      (setq-local eldoc-idle-delay 1)
    (kill-local-variable 'eldoc-idle-delay)))

(add-hook 'radian-slow-autocomplete-mode-hook #'radian-eldoc-toggle-slow)

(provide 'radian-eldoc)

;;; radian-eldoc.el ends here
