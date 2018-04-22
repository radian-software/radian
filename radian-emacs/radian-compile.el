;;; radian-compile.el --- M-x compile and friends

(use-package compile
  :straight nil
  :config

  ;; Automatically scroll the Compilation buffer as output appears,
  ;; but stop at the first error.
  (setq compilation-scroll-output 'first-error))

(provide 'radian-compile)

;;; radian-compile.el ends here
