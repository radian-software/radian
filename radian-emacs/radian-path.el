;;; radian-path.el --- Setting the $PATH correctly

(require 'radian-os)
(require 'radian-windowed)
(require 'subr-x)

;; In the terminal, the mouse and clipboard don't work properly. But
;; in windowed Emacsen, the $PATH is not necessarily set correctly!
;; You can't win.

(radian-with-operating-system macOS
  (radian-with-windowed-emacs
    (with-temp-buffer
      ;; See: man path_helper.
      (call-process "/usr/libexec/path_helper" nil t nil "-s")
      (goto-char (point-min))
      (if (search-forward-regexp "PATH=\"\\(.+\\)\"; export PATH;"
                                 nil 'noerror)
          (let ((path (match-string 1)))
            (setenv "PATH" path)
            ;; The next two statements are from the code of
            ;; exec-path-from-shell [1], and I thought they were
            ;; probably there for a good reason.
            ;;
            ;; [1]: https://github.com/purcell/exec-path-from-shell
            (setq eshell-path-env path)
            (setq exec-path (append (parse-colon-path path)
                                    (list exec-directory))))
        (warn "Could not set $PATH using /usr/libexec/path_helper"))
      ;; Sometimes path_helper also reports a MANPATH setting, but not
      ;; always.
      (when (search-forward-regexp "MANPATH=\"\\(.+\\)\"; export MANPATH;"
                                   nil 'noerror)
        (let ((manpath (match-string 1)))
          (setenv "MANPATH" manpath))))))

(provide 'radian-path)

;;; radian-path.el ends here
