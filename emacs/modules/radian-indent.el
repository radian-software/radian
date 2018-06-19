;; -*- lexical-binding: t -*-

(require 'radian-slow)

;; This package keeps indentation correct at all times. It can be a
;; phenomenal productivity booster, but only if your indentation is
;; consistent.
(use-package aggressive-indent
  :init

  ;; Allow disabling/enabling `aggressive-indent-mode' from a
  ;; file-local or directory-local variable list.
  (put 'aggressive-indent-mode 'safe-local-variable #'booleanp)

  :config

  ;; Register `aggressive-indent' in `radian-slow-indent-mode'.

  (defun radian-aggressive-indent-toggle-slow ()
    "Slow down `aggressive-indent' by disabling reindentation on save.
This is done in `radian-slow-indent-mode'."
    ;; If `aggressive-indent' hasn't been enabled yet, we might have
    ;; to wait to do the following code, so defer it using the mode
    ;; hook. Running this function is supposed to be idempotent and
    ;; generally safe, so we can do it whenever.
    (add-hook 'aggressive-indent-mode-hook
              #'radian-aggressive-indent-toggle-slow)
    (if (or radian-slow-indent-mode (not aggressive-indent-mode))
        (remove-hook 'before-save-hook
                     ;; Yes, this is a typo in `aggressive-indent'.
                     #'aggressive-indent--proccess-changed-list-and-indent
                     'local)
      (add-hook 'before-save-hook
                #'aggressive-indent--proccess-changed-list-and-indent
                nil 'local)))

  (add-hook 'radian-slow-indent-mode #'radian-aggressive-indent-toggle-slow)

  ;; Fix an extremely annoying bug in `while-no-input' that causes
  ;; `aggressive-indent' to sometimes swallow keyboard input events
  ;; when Emacs is running slowly. See [1] for discussion. The below
  ;; patch will be released in Emacs 27, so it is included below to
  ;; fix the bug on older versions.
  ;;
  ;; [1]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31692
  (el-patch-defmacro while-no-input (&rest body)
    "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
    (declare (debug t) (indent 0))
    (let ((catch-sym (make-symbol "input")))
      `(with-local-quit
         (catch ',catch-sym
	   (let ((throw-on-input ',catch-sym)
                 (el-patch-add val))
             (el-patch-wrap 2
               (setq val (or (input-pending-p)
	                     (progn ,@body))))
             (el-patch-add
               (cond
                ;; When input arrives while throw-on-input is non-nil,
                ;; kbd_buffer_store_buffered_event sets quit-flag to the
                ;; value of throw-on-input.  If, when BODY finishes,
                ;; quit-flag still has the same value as throw-on-input, it
                ;; means BODY never tested quit-flag, and therefore ran to
                ;; completion even though input did arrive before it
                ;; finished.  In that case, we must manually simulate what
                ;; 'throw' in process_quit_flag would do, and we must
                ;; reset quit-flag, because leaving it set will cause us
                ;; quit to top-level, which has undesirable consequences,
                ;; such as discarding input etc.  We return t in that case
                ;; because input did arrive during execution of BODY.
                ((eq quit-flag throw-on-input)
                 (setq quit-flag nil)
                 t)
                ;; This is for when the user actually QUITs during
                ;; execution of BODY.
                (quit-flag
                 nil)
                (t val))))))))

  :diminish "AggrIndent")

(provide 'radian-indent)
