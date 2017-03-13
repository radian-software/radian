;;; radian-clipboard.el --- Integration with the system clipboard

(require 'radian-os)
(require 'radian-windowed)

;; Like mouse integration (see `radian-mouse'), clipboard integration
;; works properly in windowed Emacs but not in terminal Emacs (at
;; least by default). This code was originally based on [1], and then
;; modified based on [2].
;;
;; [1]: https://gist.github.com/the-kenny/267162
;; [2]: http://emacs.stackexchange.com/q/26471/12534

(radian-with-operating-system macOS
  (radian-with-terminal-emacs
    (defvar radian--last-copy-to-macOS nil
      "The last text that was copied to the system clipboard.")
    (defun radian--paste-from-macOS ()
      (let ((text (shell-command-to-string "pbpaste")))
        ;; If this function returns nil then the system clipboard is
        ;; ignored and the first element in the yank ring (which, if
        ;; the system clipboard has not been modified since the last
        ;; kill, will be the same). Including this `unless' clause
        ;; prevents you from getting the same text yanked the first
        ;; time you run `yank-pop'.
        (unless (string= text radian--last-copy-to-macOS)
          text)))
    (defun radian--copy-to-macOS (text)
      ;; Setting `process-connection-type' makes Emacs use a pipe to
      ;; communicate with pbcopy, rather than a pty (which is
      ;; overkill).
      (let* ((process-connection-type nil)
             ;; The nil argument tells Emacs to discard stdout and
             ;; stderr. Note, we aren't using `call-process' here
             ;; because we want this command to be asynchronous.
             (proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof text))
      (setq radian--last-copy-to-macOS text))
    (setq interprogram-paste-function #'radian--paste-from-macOS)
    (setq interprogram-cut-function #'radian--copy-to-macOS)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

(provide 'radian-clipboard)

;;; radian-clipboard.el ends here
