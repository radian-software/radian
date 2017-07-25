;;; radian-completion.el --- Completion systems

(require 'cl-lib)
(require 'radian-appearance)
(require 'radian-package)
(require 'radian-patch)

;; This package provides a simple mechanism for recording the user's
;; command history so that it can be used to sort commands by usage.
;; It is automatically used by Ivy. Note, however, that historian.el
;; will hopefully replace smex soon, since it provides more
;; functionality in a more elegant way. See [1], [2].
;;
;; [1]: https://github.com/nonsequitur/smex
;; [2]: https://github.com/PythonNut/historian.el
(use-package smex
  :bind (("M-x" . smex)))

;; This package provides a framework for sorting choices in a
;; hopefully intelligent way based on what the user has typed in,
;; using "fuzzy matching" (i.e. "ffap" matches "find-file-at-point").
;; See [1].
;;
;; [1]: https://github.com/lewang/flx
(use-package flx)

;; Ivy is a completion and narrowing framework. What does this mean?
;; By default, Emacs has some basic tab-completion for commands,
;; files, and so on. Ivy replaces this interface by showing a list of
;; all the possible options, and narrowing it in an intelligent
;; way (using smex and flx, if they are installed) as the user inputs
;; a query. This is much faster.
(use-package ivy
  :init

  ;; Lazy-load Ivy.

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
  (ivy-mode 1)

  ;; Don't show it in the mode line (the `:diminish' below only takes
  ;; effect after the lazy-load is triggered).
  (diminish 'ivy-mode)

  :bind (;; Add a keybinding for resuming the last completion session.
         ;; The keybinding C-c C-r is suggested in the README for Ivy,
         ;; but it's overridden by `sh-mode' and `clojure-mode'.
         ("C-x C-r" . ivy-resume)

         ;; Improve the Ivy keybindings, making them more predictable.
         ;;
         ;; There are essentially three "accept" actions in Ivy: RET,
         ;; TAB, and C-j. Sometimes two or even all three of them do
         ;; the same thing, but there is one situation when there are
         ;; three different behaviors that might be desirable. This is
         ;; when you are in `counsel-find-file', and you have typed in
         ;; "foo", and there is a folder called "foobar" in the same
         ;; directory.
         ;;
         ;; In this situation, RET will `dired' the folder "foobar",
         ;; TAB will move Ivy into the "foobar" folder, and C-j will
         ;; create a new file called "foo". But only with the
         ;; keybindings below! The default behavior is different, and
         ;; weird. I think this is the most intuitive and useful.
         ;;
         ;; As always, TAB is for terminal Emacs and <tab> is for
         ;; windowed Emacs.
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("<tab>" . ivy-alt-done)
         ("C-j" . ivy-immediate-done))
  :config

  ;; Use fuzzy matching for Ivy, powered by flx, but not for Swiper
  ;; (because fuzzy matching is typically not desired in grep-style
  ;; searches, just plain 'ol regex).
  ;;
  ;; [1]: http://oremacs.com/2016/01/06/ivy-flx/
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  ;; Increase the maximum number of candidates that will be sorted
  ;; using `flx'. The default is 200, which means `flx' is almost
  ;; never used. Setting it too high (e.g. 10000) causes lag. This
  ;; seems to be a good compromise (for example, @PythonNut uses it,
  ;; see [1]).
  ;;
  ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
  (setq ivy-flx-limit 2000)

  ;; A recent (at the time of this writing) patch to Ivy improved the
  ;; default sorting of candidates in many cases, but broke smex. This
  ;; hopefully temporary hack restores the functionality of smex by
  ;; reverting to the old sorting function in `counsel-M-x'.

  (defun ivy--dumb-flx-sort (name cands)
    "Sort according to closeness to string NAME the string list CANDS."
    (condition-case nil
        (if (and cands
                 (< (length cands) ivy-flx-limit))
            (let* ((flx-name (if (string-match "^\\^" name)
                                 (substring name 1)
                               name))
                   (cands-with-score
                    (delq nil
                          (mapcar
                           (lambda (x)
                             (let ((score (flx-score x flx-name ivy--flx-cache)))
                               (and score
                                    (cons score x))))
                           cands))))
              (if cands-with-score
                  (mapcar #'cdr
                          (sort cands-with-score
                                (lambda (x y)
                                  (> (caar x) (caar y)))))
                cands))
          cands)
      (error
       cands)))

  (defun radian--advice-preserve-smex-sorting
      (counsel-M-x &optional initial-input)
    "Restore the old sorting of smex results.
This is an `:around' advice for `counsel-M-x'."
    (cl-letf (((symbol-function #'ivy--flx-sort)
               (symbol-function #'ivy--dumb-flx-sort)))
      (funcall counsel-M-x initial-input)))

  (advice-add #'counsel-M-x :around
              #'radian--advice-preserve-smex-sorting)

  :diminish ivy-mode)

;; Ivy is just a general-purpose completion framework. It can be used
;; to generate improved versions of many stock Emacs commands. This is
;; done by the Counsel library. (It also adds a few new commands, such
;; as `counsel-git-grep'.)
(use-package counsel
  :bind (;; Use Counsel for common Emacs commands.
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-load-library)
         ("C-h C-l" . counsel-find-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-x 8 RET" . counsel-unicode-char)

         ;; Introduce a few new commands that use Counsel. The
         ;; bindings are suggested by the README [1].
         ;;
         ;; [1]: https://github.com/abo-abo/swiper
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)

         ;; Interactively select a kill to yank using ivy, when using
         ;; `yank-pop'.
         ("M-y" . counsel-yank-pop)

         ;; After you have pressed M-:, you can use C-r to select a
         ;; previous entry using Counsel.
         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :config

  ;; If there is a valid file at point, pre-select in C-x C-f.
  (setq counsel-find-file-at-point t))

;; Remembers your choices in completion menus.
(use-package historian
  :demand t
  :config

  ;; Enable the functionality of historian.el.
  (historian-mode 1))

;; Uses Historian to sort Ivy candidates by frecency+flx.
(use-package ivy-historian
  :after ivy
  :config

  ;; Tweak historian weighting settings. These values are chosen
  ;; subjectively to produce good results.
  (setq ivy-historian-freq-boost-factor 500)
  (setq ivy-historian-recent-boost 500)
  (setq ivy-historian-recent-decrement 50)

  ;; Enable the functionality of historian-ivy.
  (ivy-historian-mode 1))

(provide 'radian-completion)

;;; radian-completion.el ends here
