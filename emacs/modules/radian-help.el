;; -*- lexical-binding: t -*-

(use-feature help
  :config

  ;; Suppress the "Type q in help window to delete it" hints.

  (defalias 'radian-advice-help-inhibit-hints #'ignore
    "Inhibit the \"Type q in help window to delete it\" hints.
This is an `:override' advice for
`help-window-display-message'.")

  (advice-add #'help-window-display-message :override
              #'radian-advice-help-inhibit-hints)

  ;; Prevent the prompt for confirmation before reverting a help
  ;; buffer (default keybinding is `g').

  (defun radian-advice-help-disable-revert-prompt
      (help-mode-revert-buffer ignore-auto _noconfirm)
    "Don't ask for confirmation before reverting help buffers.
This is an `:around' advice for `help-mode-revert-buffer'."
    (funcall help-mode-revert-buffer ignore-auto 'noconfirm))

  (advice-add #'help-mode-revert-buffer :around
              #'radian-advice-help-disable-revert-prompt))

;; Package `help-fns+', although suffering from a number of design
;; infelicities, provides one helpful feature: a way to view a keymap
;; in a human-readable manner.
(use-package help-fns+
  :bind (("C-h M-k" . describe-keymap)
         ;; Prevent help-fns+ from overriding this built-in
         ;; keybinding:
         ("C-h o" . describe-symbol)))

(use-feature cus-edit
  :config

  ;; Don't show the search box in Custom.
  (setq custom-search-field nil))

(provide 'radian-help)
