;;; radian-pairs.el --- Paired delimiter handling

(require 'radian-package)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; `smartparens'.
(setq blink-matching-paren nil)

;; Insert and manipulate paired delimiters.
(use-package smartparens
  :init

  ;; Load the default configuration, including `with-eval-after-load'
  ;; forms for more specific configurations in other modes.
  (require 'smartparens-config)

  :config

  ;; Enable the functionality of Smartparens everywhere.
  (smartparens-global-mode +1)

  ;; Smartparens' Paredit emulation is missing some bindings, so we
  ;; re-add them here.
  (radian-alist-set* "M-?" #'sp-convolute-sexp sp-paredit-bindings)
  (radian-alist-set* "M-j" #'sp-join-sexp sp-paredit-bindings)

  ;; Enable some default keybindings for Smartparens.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent paired delimiters from ever becoming unpaired, in Lisp
  ;; modes.
  (dolist (mode sp-lisp-modes)
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook #'smartparens-strict-mode)))

  ;; Prevent all highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.

  (with-eval-after-load 'org
    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (with-eval-after-load 'org-agenda
    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode)))

(provide 'radian-pairs)

;;; radian-pairs.el ends here
