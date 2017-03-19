;;; radian-window.el --- Window management

(require 'radian-package)

;; Use S-left, S-right, S-up, and S-down to move between windows. This
;; is much more convenient and efficient than using C-x o.
(windmove-default-keybindings)

;; Use C-c left and C-c right to undo and redo (actually, cancel a
;; sequence of undos) through window layouts. For instance, you can
;; use C-x 1 to focus on a particular window, then return to your
;; previous layout with C-c left.
(winner-mode 1)

;; Provides simple commands to mirror, rotate, and transpose Emacs
;; windows: `flip-frame', `flop-frame', `transpose-frame',
;; `rotate-frame-clockwise', `rotate-frame-anticlockwise',
;; `rotate-frame'.
(use-package transpose-frame)

;; Provides simple commands to swap Emacs windows: `buf-move-up',
;; `buf-move-down', `buf-move-left', `buf-move-right'.
(use-package buffer-move)

;; Provides more intuitive behavior for C-x <left> and C-x <right>.
(use-package iflipb
  :bind (("C-x <left>" . iflipb-next-buffer)
         ("C-x <right>" . iflipb-previous-buffer))
  :config

  ;; Don't skip buffers that start with an asterisk.
  (setq iflipb-ignore-buffers nil)

  ;; Allow the use of C-x <right> as an "undo" operation even after
  ;; breaking the C-x <arrow> chain and running another command.
  (setq iflipb-permissive-flip-back t)

  ;; Don't show the state of the buffer switching in the minibuffer,
  ;; because the buffers are shown from left to right. (This means
  ;; that C-x <left> moves right and C-x <right> moves left, which is
  ;; confusing.) This means that `iflipb' is no longer an alt-tab
  ;; emulator, but just a silent improvement on C-x <left> and C-x
  ;; <right>.
  ;;
  ;; The first advice is to make `iflipb-format-buffers' return nil;
  ;; the second is to prevent `iflip-message' from trying to print a
  ;; nil string.

  (defalias 'radian--advice-inhibit-iflipb-format-buffers #'ignore
    "Inhibit the display of the buffer list by `iflipb'.
This is an `:override' advice for `iflipb-format-buffers'. See
also `radian--advice-inhibit-iflipb-display-buffers'.")
  (defalias 'radian--advice-inhibit-iflipb-display-buffers #'identity
    "Inhibit the display of the buffer list by `iflipb'.
This is a `:before-while' advice for `iflipb-message'. See also
`radian--advice-inhibit-iflipb-format-buffers'.")

  (advice-add #'iflipb-format-buffers :override #'ignore)
  (advice-add #'iflipb-message :before-while #'identity))

(provide 'radian-window)

;;; radian-window.el ends here
