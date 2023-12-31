;; This file has code that is evaluated in CI before the indentation
;; of Radian is checked. This is helpful because it allows us to
;; ensure that various things are indented correctly if they require
;; some setup for Emacs to know how to do the right thing.

;; Load libraries whose definitions we need to overwrite.
(require 'subr-x)

;; The indentation of `define-key' has for some reason changed in
;; Emacs 29 when it was deprecated in favor of `keymap-set'. Maybe
;; that is a bug and they will change it, but for now, force the
;; indentation to the backwards-compatible version.
(put #'define-key 'lisp-indent-function 'defun)

;; The indentation of `thread-first' changed from (indent 1) to
;; (indent 0) in Emacs 28. Use the later version.
(put #'thread-first 'lisp-indent-function 0)
