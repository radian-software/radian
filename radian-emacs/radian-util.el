;;; radian-util.el --- Miscellaneous utility functions

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

(provide 'radian-util)

;;; radian-util.el ends here
