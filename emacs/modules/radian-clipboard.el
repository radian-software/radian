;; -*- lexical-binding: t -*-

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

    (defvar radian-clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
This is used to prevent duplicate entries in the kill ring.")

    (defun radian-clipboard-paste ()
      "Return the contents of the macOS clipboard, as a string."
      (let* (;; Setting `default-directory' to a directory that is
             ;; sure to exist means that this code won't error out
             ;; when the directory for the current buffer does not
             ;; exist.
             (default-directory "/")
             ;; Command pbpaste returns the clipboard contents as a
             ;; string.
             (text (shell-command-to-string "pbpaste")))
        ;; If this function returns nil then the system clipboard is
        ;; ignored and the first element in the kill ring (which, if
        ;; the system clipboard has not been modified since the last
        ;; kill, will be the same). Including this `unless' clause
        ;; prevents you from getting the same text yanked the first
        ;; time you run `yank-pop'. (Of course, this is less relevant
        ;; due to `counsel-yank-pop', but still arguably the correct
        ;; behavior.)
        (unless (string= text radian-clipboard-last-copy)
          text)))

    (defun radian-clipboard-copy (text)
      "Set the contents of the macOS clipboard to given TEXT string."
      (let* (;; Setting `default-directory' to a directory that is
             ;; sure to exist means that this code won't error out
             ;; when the directory for the current buffer does not
             ;; exist.
             (default-directory "/")
             ;; Setting `process-connection-type' makes Emacs use a pipe to
             ;; communicate with pbcopy, rather than a pty (which is
             ;; overkill).
             (process-connection-type nil)
             ;; The nil argument tells Emacs to discard stdout and
             ;; stderr. Note, we aren't using `call-process' here
             ;; because we want this command to be asynchronous.
             ;;
             ;; Command pbcopy writes stdin to the clipboard until it
             ;; receives EOF.
             (proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))
      (setq radian-clipboard-last-copy text))

    (setq interprogram-paste-function #'radian-clipboard-paste)
    (setq interprogram-cut-function #'radian-clipboard-copy)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

(provide 'radian-clipboard)
