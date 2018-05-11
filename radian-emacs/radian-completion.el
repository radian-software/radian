;;; radian-completion.el --- Completion systems

(require 'radian-patch)
(require 'radian-slow)

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

  ;; Allow arbitrary substring matching (not requiring matches to
  ;; start at the beginning of a candidate) for all commands, by
  ;; default.
  (setq ivy-initial-inputs-alist nil)

  :diminish ivy-mode)

;; Package `counsel' provides purpose-built replacements for many
;; built-in Emacs commands that use enhanced configurations of `ivy'
;; to provide extra features.
(use-package counsel
  :init

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

  (defvar radian-counsel-sort-commands
    '(counsel-find-library counsel-find-file)
    "List of commands which should have their candidates always sorted.")

  (cl-defun radian-advice-counsel-override-sort
      (ivy-read prompt collection &rest rest &key caller &allow-other-keys)
    "Delegate to `ivy-read', overriding `:sort' depending on CALLER.
Specifically, if CALLER appears in
`radian-counsel-sort-commands', then override `:sort' to non-nil
unconditionally.

This is an `:around' advice for `ivy-read'."
    (when (memq caller radian-counsel-sort-commands)
      (setq rest (append '(:sort t) rest)))
    (apply ivy-read prompt collection rest))

  (advice-add #'ivy-read :around #'radian-advice-counsel-override-sort)

  :diminish counsel-mode)

(defvar radian-company-backends-global
  '(company-capf
    company-files
    (company-dabbrev-code company-keywords)
    company-dabbrev)
  "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :demand t
  :bind (;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'. You might think this could be put
         ;; in the `:bind*' declaration below, but it seems that
         ;; `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;;; The following are keybindings that take effect whenever
         ;;; the completions menu is visible, even if the user has not
         ;;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection. Note that
         ;; <tab> is for windowed Emacs and TAB is for terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Prevent SPC from ever triggering a completion.
         ("SPC" . nil)

         ;;; The following are keybindings that only take effect if
         ;;; the user has explicitly interacted with Company.

         ;; You'll want to take careful note of the repetition of
         ;; `:map' here. It's because `use-package' parses `:bind'
         ;; forms by sectioning them at each *group* of keywords,
         ;; where a new keyword cancels all other keywords, and then
         ;; all consecutive keywords apply to new bindings. See [1]
         ;; for discussion of this.
         ;;
         ;; [1]: https://github.com/jwiegley/use-package/issues/334
         :map company-active-map
         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then do the same for the up and down arrows. Note that
         ;; we use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.

         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. Here we
          ;; make sure that no minor modes override this keybinding.
          ("M-TAB" . company-manual-begin))

  :diminish company-mode
  :config

  ;; Show completions instantly, rather than after half a second.
  (setq company-idle-delay 0)

  ;; Show completions after typing a single character, rather than
  ;; after typing three characters.
  (setq company-minimum-prefix-length 1)

  ;; Show a maximum of 10 suggestions. This is the default but I think
  ;; it's best to be explicit.
  (setq company-tooltip-limit 10)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Prevent Company completions from being lowercased in the
  ;; completion menu. This has only been observed to happen for
  ;; comments and strings in Clojure. (Although in general it will
  ;; happen wherever the Dabbrev backend is invoked.)
  (setq company-dabbrev-downcase nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make company-dabbrev case-sensitive. Case insensitivity seems
  ;; like a great idea, but it turns out to look really bad when you
  ;; have domain-specific words that have particular casing.
  (setq company-dabbrev-ignore-case nil)

  ;; Register `company' in `radian-slow-autocomplete-mode'.

  (defun radian-company-toggle-slow ()
    "Slow down `company' by turning up the delays before completion starts.
This is done in `radian-slow-autocomplete-mode'."
    (if radian-slow-autocomplete-mode
        (progn
          (setq-local company-idle-delay 1)
          (setq-local company-minimum-prefix-length 3))
      (kill-local-variable 'company-idle-delay)
      (kill-local-variable 'company-minimum-prefix-length)))

  (add-hook 'radian-slow-autocomplete-mode-hook #'radian-company-toggle-slow)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (with-eval-after-load 'yasnippet
    ;; FIXME: this is all a horrible hack, can it be done with
    ;; `bind-key' instead?
    ;;
    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key'
    ;; and `define-key'. It's a hack, and I'd like to find a
    ;; built-in function that accomplishes the same thing while
    ;; taking care of any edge cases I might have missed in this
    ;; ad-hoc solution.
    (defun radian-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (setq radian-yas-company-keymap
          ;; It starts out as a copy of `yas-keymap', and then we
          ;; merge in all of the bindings from
          ;; `company-active-map'.
          (let ((keymap (copy-keymap yas-keymap)))
            (map-keymap
             (lambda (event company-cmd)
               (let* ((event (radian-normalize-event event))
                      (yas-cmd (lookup-key yas-keymap event)))
                 ;; Here we use an extended menu item with the
                 ;; `:filter' option, which allows us to
                 ;; dynamically decide which command we want to
                 ;; run when a key is pressed.
                 (define-key keymap event
                   `(menu-item
                     nil ,company-cmd :filter
                     (lambda (cmd)
                       ;; There doesn't seem to be any obvious
                       ;; function from Company to tell whether or
                       ;; not a completion is in progress (à la
                       ;; `company-explicit-action-p'), so I just
                       ;; check whether or not `company-my-keymap'
                       ;; is defined, which seems to be good
                       ;; enough.
                       (if company-my-keymap
                           ',company-cmd
                         ',yas-cmd))))))
             company-active-map)
            keymap))

    ;; The function `yas--make-control-overlay' uses the current
    ;; value of `yas-keymap' to build the Yasnippet overlay, so to
    ;; override the Yasnippet keymap we only need to dynamically
    ;; rebind `yas-keymap' for the duration of that function.
    (defun radian-advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      "Allow `company' to override `yasnippet'.
This is an `:around' advice for `yas--make-control-overlay'."
      (let ((yas-keymap radian-yas-company-keymap))
        (apply yas--make-control-overlay args)))

    (advice-add #'yas--make-control-overlay :around
                #'radian-advice-company-overrides-yasnippet))

  ;; Turn on Company everywhere.
  (global-company-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts. It is not published to MELPA, so we
;; must define a recipe here.
(use-package prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("prescient.el"))
  :demand t
  :after ivy
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Package `ivy-prescient' provides intelligent sorting and filtering
;; for candidates in Ivy menus.
(use-package ivy-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("ivy-prescient.el"))
  :demand t
  :after ivy
  :config

  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("company-prescient.el"))
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

(provide 'radian-completion)

;;; radian-completion.el ends here
