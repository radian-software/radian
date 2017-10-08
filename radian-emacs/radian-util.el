;;; radian-util.el --- Miscellaneous utility functions

(require 'radian-os)
(require 'subr-x)

;; These functions will become unnecessary in Emacs 26.1, which
;; extends `map-put' to have a TESTFN argument.

(defun radian-alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets
the cdr of the first matching association in the list. It does
not create duplicate associations. By default, key comparison is
done with `equal'. However, if SYMBOL is non-nil, then `eq' is
used instead.

This method may mutate the original alist, but you still need to
use the return value of this method instead of the original
alist, to ensure correct results."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

(defmacro radian-alist-set* (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
ALIST must be a literal symbol naming a variable holding an
alist. That variable will be re-set using `setq'. By default, key
comparison is done with `equal'. However, if SYMBOL is non-nil,
then `eq' is used instead. See also `radian-alist-set'."
  `(setq ,alist (radian-alist-set ,key ,val ,alist ,symbol)))

(defun radian-insert-after (insert-elt before-elt list &optional testfn)
  "Insert INSERT-ELT after BEFORE-ELT in LIST, returning copy of LIST.
The original LIST is not modified. If BEFORE-ELT is not in LIST,
it is inserted at the end. Element comparison is done with
TESTFN, which defaults to `eq'. See also `radian-insert-before'
and `radian-insert-after*'."
  (let ((testfn (or testfn #'eq)))
    (cond
     ((null list)
      (list insert-elt))
     ((funcall testfn before-elt (car list))
      (append (list (car list) insert-elt) (copy-sequence (cdr list))))
     (t (cons (car list)
              (radian-insert-after
               insert-elt before-elt (cdr list) testfn))))))

(defmacro radian-insert-after* (insert-elt before-elt list &optional testfn)
  "Insert INSERT-ELT after BEFORE-ELT in LIST, returning copy of LIST.
LIST must be a literal symbol naming a variable holding a list.
That variable will be re-set using `setq'. Element comparison is
done with TESTFN, which defaults to `eq'. See also
`radian-insert-after' and `radian-insert-before'."
  `(setq ,list (radian-insert-after ,insert-elt ,before-elt ,list ,testfn)))

(defun radian-insert-before (insert-elt after-elt lst &optional testfn)
  "Insert INSERT-ELT before AFTER-ELT in LIST, returning copy of LIST.
The original LIST is not modified. If BEFORE-ELT is not in LIST,
it is inserted at the beginning. Element comparison is done with
TESTFN, which defaults to `eq'. See also `radian-insert-after'
and `radian-insert-before*'."
  (nreverse (radian-insert-after insert-elt after-elt (reverse lst) testfn)))

(defmacro radian-insert-before* (insert-elt after-elt list &optional testfn)
  "Insert INSERT-ELT before AFTER-ELT in LIST, returning copy of LIST.
LIST must be a literal symbol naming a variable holding a list.
That variable will be re-set using `setq'. Element comparison is
done with TESTFN, which defaults to `eq'. See also
`radian-insert-before' and `radian-insert-after*'."
  `(setq ,list (radian-insert-before ,insert-elt ,after-elt ,list ,testfn)))

(defun radian-managed-p (filename)
  "Return non-nil if FILENAME is managed by Radian.
This means that FILENAME is a symlink whose target is inside
`radian-directory'."
  (and radian-directory
       (string-prefix-p radian-directory (file-truename filename)
                        ;; The filesystem on macOS is case-insensitive
                        ;; but case-preserving, so we have to compare
                        ;; case-insensitively in that situation.
                        (eq radian-operating-system 'macOS))))

(provide 'radian-util)

;;; radian-util.el ends here
