;;; radian-completion.el --- Completion systems

(require 'cl-lib)
(require 'radian-appearance)
(require 'radian-patch)

;; Package `ivy' provides a user interface for choosing from a list of
;; options by typing a query to narrow the list, and then selecting
;; one of the remaining candidates. This offers a significant
;; improvement over the default Emacs interface for candidate
;; selection.
(use-package ivy
  :init

  ;; Lazy-load `ivy'.

  (el-patch-feature ivy)

  (el-patch-defvar ivy-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap switch-to-buffer]
        'ivy-switch-buffer)
      (define-key map [remap switch-to-buffer-other-window]
        'ivy-switch-buffer-other-window)
      map)
    "Keymap for `ivy-mode'.")

  (el-patch-define-minor-mode ivy-mode
    "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}"
    :group 'ivy
    :global t
    :keymap ivy-mode-map
    :lighter " ivy"
    (if ivy-mode
        (progn
          (setq completing-read-function 'ivy-completing-read)
          (el-patch-splice 2
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region))))
      (setq completing-read-function 'completing-read-default)
      (setq completion-in-region-function 'completion--in-region)))

  ;; Use Ivy for `completing-read'.
  (ivy-mode +1)

  ;; Don't show it in the mode line (the `:diminish' below only takes
  ;; effect after the lazy-load is triggered).
  (diminish 'ivy-mode)

  :bind (;; Add a keybinding for resuming the last completion session.
         ;; The keybinding C-c C-r is suggested in the README for Ivy,
         ;; but it's overridden by `sh-mode' and `clojure-mode'.
         ("C-x C-r" . ivy-resume))
  :config

  ;; `prescient' does not support putting a caret ^ at the beginning
  ;; of a query to ensure that match starts at the beginning of the
  ;; candidate, so don't put ^ as the initial input for any command.
  (setq ivy-initial-inputs-alist nil)

  :diminish ivy-mode)

;; Package `counsel' provides purpose-built replacements for many
;; built-in Emacs commands that use enhanced configurations of `ivy'
;; to provide extra features.
(use-package counsel
  :init

  (el-patch-feature counsel)

  ;; Lazy-load `counsel'.

  (el-patch-defvar counsel-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (binding
               '((execute-extended-command . counsel-M-x)
                 (describe-bindings . counsel-descbinds)
                 (describe-function . counsel-describe-function)
                 (describe-variable . counsel-describe-variable)
                 (describe-face . counsel-describe-face)
                 (list-faces-display . counsel-faces)
                 (find-file . counsel-find-file)
                 (find-library . counsel-find-library)
                 (imenu . counsel-imenu)
                 (load-library . counsel-load-library)
                 (load-theme . counsel-load-theme)
                 (yank-pop . counsel-yank-pop)
                 (info-lookup-symbol . counsel-info-lookup-symbol)
                 (pop-to-mark-command . counsel-mark-ring)
                 (bookmark-jump . counsel-bookmark)))
        (define-key map (vector 'remap (car binding)) (cdr binding)))
      map)
    "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements.")

  (el-patch-defcustom counsel-mode-override-describe-bindings nil
    "Whether to override `describe-bindings' when `counsel-mode' is active."
    :group 'ivy
    :type 'boolean)

  (el-patch-define-minor-mode counsel-mode
    "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements. "
    :group 'ivy
    :global t
    :keymap counsel-mode-map
    :lighter " counsel"
    (if counsel-mode
        (progn
          (when (and (fboundp 'advice-add)
                     counsel-mode-override-describe-bindings)
            (advice-add #'describe-bindings :override #'counsel-descbinds))
          (define-key minibuffer-local-map (kbd "C-r")
            'counsel-minibuffer-history))
      (when (fboundp 'advice-remove)
        (advice-remove #'describe-bindings #'counsel-descbinds))))

  ;; Use customized Ivy configurations for built-in Emacs commands.
  (counsel-mode +1)

  ;; Diminish the lazy-loaded version of `counsel-mode'.
  (diminish 'counsel-mode)

  :config

  (el-patch-defun counsel-find-library ()
    "Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
    (interactive)
    (let ((cands (counsel-library-candidates)))
      (ivy-read "Find library: " cands
                :action #'counsel--find-symbol
                :keymap counsel-describe-map
                :caller 'counsel-find-library
                (el-patch-add
                  :sort t))))

  :diminish t)

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts. It is not published to MELPA, so we
;; must define a recipe here.
(use-package prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("prescient.el")))

;; Package `ivy-prescient' provides intelligent sorting and filtering
;; for candidates in Ivy menus.
(use-package ivy-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("ivy-prescient.el"))
  :demand t
  :after ivy
  :config

  ;; Use `prescient' for Ivy commands.
  (ivy-prescient-mode +1))

(provide 'radian-completion)

;;; radian-completion.el ends here
