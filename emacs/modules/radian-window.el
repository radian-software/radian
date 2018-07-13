;; -*- lexical-binding: t -*-

;; Split windows horizontally (into tall subwindows) rather than
;; vertically (into wide subwindows) by default.
(el-patch-defun split-window-sensibly (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
WINDOW defaults to the currently selected window.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.

You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.

In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.

Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
  (let ((window (or window (selected-window))))
    (or (el-patch-let
            (($fst (and (window-splittable-p window)
                        ;; Split window vertically.
                        (with-selected-window window
                          (split-window-below))))
             ($snd (and (window-splittable-p window t)
                        ;; Split window horizontally.
                        (with-selected-window window
                          (split-window-right)))))
          (el-patch-swap $fst $snd)
          (el-patch-swap $snd $fst))
        (and
         ;; If WINDOW is the only usable window on its frame (it
         ;; is the only one or, not being the only one, all the
         ;; other ones are dedicated) and is not the minibuffer
         ;; window, try to split it s/vertically/horizontally
         ;; disregarding the value of `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let (((el-patch-swap split-height-threshold
                               split-width-threshold)
                0))
           (when (window-splittable-p window)
             (with-selected-window window
               ((el-patch-swap split-window-below split-window-right)))))))))

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

;; Package `ibuffer' provides a replacement for the default binding of
;; C-x C-b to `list-buffers'.
(use-package ibuffer
  :straight nil
  :bind (("C-x C-b" . ibuffer)))

(provide 'radian-window)
