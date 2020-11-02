;; -*- lexical-binding: t -*-

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Radian standard library

(defmacro flet! (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.

\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

(defun radian-quiet (func &rest args)
  "Call FUNC with ARGS without printing any output.
This can be added as an `:around' advice to any other function."
  (quiet! (apply func args)))

;;; Commands

(defadvice! radian--ad-quoted-insert-allow-quit (quoted-insert &rest args)
  "Allow quitting out of \\[quoted-insert] with \\[keyboard-quit]."
  :around #'quoted-insert
  (flet! ((defun insert-and-inherit (&rest args)
            (dolist (arg args)
              (when (equal arg ?\C-g)
                (signal 'quit nil)))
            (apply insert-and-inherit args)))
    (apply quoted-insert args)))

;; Configure `which-key' so that it doesn't do anything unless you
;; type C-h, and in that case it immediately shows you suggestions for
;; what to type next. See
;; <https://github.com/justbur/emacs-which-key#manual-activation>.
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay most-positive-fixnum)
(setq which-key-idle-secondary-delay 1e-100)

;;; Editing
;;;; Copy/paste

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop' or
;; equivalent.
(setq save-interprogram-paste-before-kill t)

(defadvice! radian--ad-stop-kill-at-whitespace (kill-line &rest args)
  "Prevent `kill-line' from killing through whitespace to a newline.
This affects the case where you kill a line when point is
followed by some whitespace and then a newline. Without this
advice, `kill-line' will kill both the whitespace and the
newline, which is inconsistent with its behavior when the
whitespace is replaced with non-whitespace. With this advice,
`kill-line' will kill just the whitespace, and another invocation
will kill the newline."
  :around #'kill-line
  (let ((show-trailing-whitespace t))
    (apply kill-line args)))

;; No idea why copying a register to the kill ring doesn't have a
;; built-in command. It seems extremely core to me.
(defun radian-copy-register (register)
  "Copy the contents of Emacs register named REGISTER to the kill ring."
  (interactive (list (register-read-with-preview "Copy register: ")))
  (unless (get-register register)
    (user-error "Register does not contain text"))
  (kill-new (get-register register))
  (message "Copied %S chars to kill ring" (length (get-register register))))

;; Not to worry, this sequence is originally bound to
;; `insert-register' which also has C-x r i.
(map! ("C-x r g" #'radian-copy-register))

;;;; Text formatting

(map! ("M-c" #'capitalize-dwim
       "M-l" #'downcase-dwim
       "M-u" #'upcase-dwim))

(map! ("C-M-<up>" #'drag-stuff-up
       "C-M-<down>" #'drag-stuff-down))

;;;; Text wrap

;; Doom sets this to 80, an increase from the default of 70. I
;; personally find that a bridge too far, because everybody else's
;; Emacs is configured to a default of 70 and it's perfectly
;; serviceable. We should encourage a standard.
(setq-default fill-column 70)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

(defadvice! radian--ad-auto-fill-only-text (func &rest args)
  :around #'internal-auto-fill
  "Only perform auto-fill in text, comments, or docstrings."
  (cl-block nil
    ;; Don't auto-fill on the first line of a docstring, since it
    ;; shouldn't be wrapped into the body.
    (when (and (derived-mode-p #'emacs-lisp-mode)
               (eq (get-text-property (point) 'face) 'font-lock-doc-face)
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p "[[:space:]]*\"")))
      (cl-return))
    (when (and (derived-mode-p 'text-mode)
               (not (derived-mode-p 'yaml-mode)))
      (apply func args)
      (cl-return))
    ;; Inspired by <https://emacs.stackexchange.com/a/14716/12534>.
    (when-let ((faces (save-excursion
                        ;; In `web-mode', the end of the line isn't
                        ;; fontified, so we have to step backwards by
                        ;; one character before checking the
                        ;; properties.
                        (ignore-errors
                          (backward-char))
                        (get-text-property (point) 'face))))
      (unless (listp faces)
        (setq faces (list faces)))
      (when (cl-some
             (lambda (face)
               (memq face '(font-lock-comment-face
                            font-lock-comment-delimiter-face
                            font-lock-doc-face
                            web-mode-javascript-comment-face)))
             faces)
        ;; Fill Elisp docstrings to the appropriate column. Why
        ;; docstrings are filled to a different column, I don't know.
        (let ((fill-column (if (and
                                (derived-mode-p #'emacs-lisp-mode)
                                (memq 'font-lock-doc-face faces))
                               emacs-lisp-docstring-fill-column
                             fill-column)))
          (apply func args))))))

(defun radian--do-auto-fill ()
  "Replacement for `do-auto-fill' that respects `normal-auto-fill-function'.
The reason we need this is that in order to enable auto-fill
globally, we are supposed to set the default value of variable
`auto-fill-function'. However, some major modes set
`normal-auto-fill-function' (itself normally set to
`do-auto-fill', which is what we generally set the default value
of variable `auto-fill-function' to), expecting `auto-fill-mode'
to be enabled afterwards (which copies the value of
`normal-auto-fill-function' into variable `auto-fill-function').
However, since we enable auto-fill globally by means of setting
variable `auto-fill-function' directly, this setting gets lost.
The workaround is to set variable `auto-fill-function' globally
to a function which looks up the value of
`normal-auto-fill-function' \(generally just `do-auto-fill') and
calls that. This is a slight inversion of the usual flow of
control and might make you slightly uncomfortable, but we'll just
have to live with it :3"
  (funcall normal-auto-fill-function))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function #'radian--do-auto-fill) 

;;;; Delimiter manipulation

;; Bind M-(, M-", C-<left>, C-<right>, M-<up>, M-<down>, M-r, M-s,
;; M-j, M-S, M-?. 
(sp-use-paredit-bindings)

;; Don't bind M-<left> or M-<right> since these are used by
;; `buf-move'. Bind M-[ as an analogue to M-(, but not in terminal
;; mode where such a binding would break escape sequences. We don't do
;; M-{ because that is already bound to `backward-paragraph'. Bind
;; C-M-a and C-M-e back to their original values, after they were
;; mapped to `sp-beginning-of-sexp' and `sp-end-of-sexp' by Doom.
(map! (:map smartparens-mode-map
       "M-<left>" nil
       "M-<right>" nil
       (:when (display-graphic-p)
        "M-[" #'sp-wrap-square)
       "C-M-a" #'beginning-of-defun
       "C-M-e" #'end-of-defun))

(defun radian-kill-line-or-sexp (&optional arg)
  "Act as `kill-line' or `sp-kill-hybrid-sexp' depending on mode."
  (interactive "P")
  (if (apply #'derived-mode-p sp-lisp-modes)
      (sp-kill-hybrid-sexp arg)
    (kill-line arg)))

;; This means C-k will kill the sexp following point, rather than
;; totally breaking everything, when in Lisp modes.
(map! ([remap kill-line] #'radian-kill-line-or-sexp))

(pushnew! sp-ignore-modes-list #'org-mode #'org-agenda-mode)

;;;; Undo/redo

;; Undo-Tree rebinds C-/ automatically, but since Emacs' default undo
;; system is so broken there is no key sequence for redo. M-/ seems
;; like a reasonable one so we choose that. We don't need
;; `dabbrev-expand'.
(map! ("M-/" #'undo-tree-redo))

;; Disable undo-in-region. It sounds like a cool feature, but
;; unfortunately the implementation is very buggy and usually causes
;; you to lose your undo history if you use it by accident, even once.
(setq undo-tree-enable-undo-in-region nil)

;;; Navigation
;;;; Scrolling

;; Doom configures this variable to a large value, which has the
;; effect of making it so that when you do a search and the result is
;; off-screen, Emacs only scrolls just enough to bring it to the edge
;; of the screen (where, of course, you can barely see it). Reset the
;; option so that Emacs will instead scroll point to the center of the
;; window.
(setq scroll-conservatively 0)

;;;; Motion

;; Make `beginning-of-line' and `end-of-line' idempotent, instead of
;; toggling back and forth between the true line beginning/end and
;; some intermediate position dependent on indentation and line
;; comments. This does not affect behavior in Org, where we actually
;; do desire that behavior.
(map! ([remap doom/backward-to-bol-or-indent] #'beginning-of-line
       [remap doom/forward-to-last-non-comment-or-eol] #'end-of-line))

;; Make `forward-word' and `backward-word' stop at capitalization
;; changes within CamelCase symbols.
(global-subword-mode +1)

;;;; Search

;; By default Doom binds C-s to `counsel-minibuffer-history', which
;; conflicts with CTRLF.
(map! (:map minibuffer-local-map "C-s" nil))

;; Enable globally. This remaps C-s, C-r, C-M-s, C-M-r as well as some
;; additional Isearch bindings.
(ctrlf-mode +1)

;;;;; Find-and-replace

(defun radian-query-replace-literal ()
  "Do a literal query-replace using `visual-regexp'."
  (interactive)
  (let ((vr/plain 'emacs-plain))
    (call-interactively #'vr/query-replace)))

(map! ([remap query-replace] #'vr/query-replace
       [remap query-replace-regexp] #'radian-query-replace-literal))

;;;; Projects

;; Do not use separate workspaces for each project. We do not want our
;; window configuration to keep changing unexpectedly when we switch
;; projects.
(setq +workspaces-on-switch-project-behavior nil)

;;; Candidate selection

;; FIXME

;;; Files

(advice-add #'recentf-cleanup :around #'radian-quiet)
(advice-add #'recentf-load-list :around #'radian-quiet)
(advice-add #'recentf-save-list :around #'radian-quiet)

(defun radian-set-executable-bit (&optional unset)
  "Set the executable bit on the current file.
UNSET non-nil or prefix arg means to unset it instead."
  (interactive "P")
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (with-demoted-errors "Could not set permissions: %S"
    (set-file-modes buffer-file-name (file-modes-symbolic-to-number
                                      (if unset
                                          "-x"
                                        "+x")
                                      (file-modes buffer-file-name)))
    (message "Executable permission %s"
             (if unset "disabled" "enabled"))))

(defun radian-unset-executable-bit (&optional set)
  "Set the executable bit on the current file.
SET non-nil or prefix arg means to set it instead."
  (interactive "P")
  (radian-set-executable-bit (not set)))

;; The only reasonable bindings for these commands are 'x' and 'X',
;; but those are already bound, so we move the existing commands to
;; 's' and 'S'.
(map! (:leader :prefix "f"
       :desc "Open scratch buffer" "s" #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer" "S" #'doom/switch-to-scratch-buffer
       :desc "Set executable bit" "x" #'radian-set-executable-bit
       :desc "Unset executable bit" "X" #'radian-unset-executable-bit))

;; Add an easy way to revert buffers, which is useful for changing
;; major mode, clearing up garbage, etc. In very rare cases it is
;; necessary to kill and re-create the buffer to get rid of something,
;; but otherwise `revert-buffer' suffices.
(map! (:leader "f v" #'revert-buffer))

;; Doom disables `auto-revert-mode', instead managing autorevert
;; itself when you switch to a buffer or Emacs gains focus. The quoted
;; reason is to avoid unnecessary resource usage when many buffers are
;; open. But there is an easy workaround: only have `auto-revert-mode'
;; enabled for buffers that are currently open in a window (probably
;; not more than four). We get the best of both worlds. Although since
;; it is a bit nontrivial to deal with setting filesystem watches for
;; only some buffers, we instead use polling which is not too bad.

;; Decrease the interval from 5s to 0.25s.
(setq auto-revert-interval 0.25)

;; Yes, autorevert Dired buffers too. The docstring warns about
;; potential performance problems but this should not be an issue
;; since we only revert visible buffers. 
(setq global-auto-revert-non-file-buffers t)

(defun radian--autorevert-inhibit-p (buffer)
  "Return non-nil if autorevert should be inhibited for BUFFER."
  (or (null (get-buffer-window buffer))
      (with-current-buffer buffer
        (and buffer-file-name
             (file-remote-p buffer-file-name)))))

(if (version<= "27" emacs-version)

    (defadvice! radian--ad-autorevert-only-visible (bufs)
      "Inhibit `autorevert' for buffers not displayed in any window."
      :filter-return #'auto-revert--polled-buffers
      (cl-remove-if #'radian--autorevert-inhibit-p bufs))

  (defadvice! radian--ad-autorevert-only-visible
    (auto-revert-buffers &rest args)
    "Inhibit `autorevert' for buffers not displayed in any window."
    :around #'auto-revert-buffers
    (flet! ((defun buffer-list (&rest args)
              (cl-remove-if
               #'radian--autorevert-inhibit-p
               (apply buffer-list args))))
      (apply auto-revert-buffers args))))

;; Enable it.
(global-auto-revert-mode +1)

;; And disable Doom's alternative implementation, we don't really need
;; it and disabling it should reduce latency when switching buffers
;; and such.
(advice-add #'doom-auto-revert-buffer-h :override #'ignore)

;; This function can be added onto various hooks such as in Dired.
(defun radian--autorevert-silence ()
  "Silence messages from `auto-revert-mode' in the current buffer."
  (setq-local auto-revert-verbose nil))

(defun radian-bookmark (keys filename &optional name)
  "Create a shortcut to find the given file (absolute path).
The shortcut is bound under leader+e plus the given keys, in the
format returned by `kbd'. With a prefix argument, the shortcut
will find the file in a new window."
  (let* ((filename (expand-file-name filename))
         (fn-name (intern
                   (concat
                    "radian-bookmark/"
                    (replace-regexp-in-string
                     "^-\\|-$" ""
                     (replace-regexp-in-string
                      "-+" "-"
                      (or
                       name
                       (replace-regexp-in-string
                        "[^a-z0-9]" "-"
                        (downcase
                         (abbreviate-file-name
                          filename))))))))))
    (eval
     `(progn
        (defun ,fn-name (&optional in-other-window)
          ,(format "Find file %s." (abbreviate-file-name filename))
          (interactive "P")
          (if in-other-window
              (find-file-other-window ,filename)
            (find-file ,filename)))
        (map! (:leader :prefix ("d" . "bookmarks") ,keys #',fn-name))))))

(radian-bookmark "d c" "~/.doom.d/config.el" "doom-config")
(radian-bookmark "d g" "~/.gitconfig")
(radian-bookmark "d i" "~/.doom.d/init.el" "doom-init")
(radian-bookmark "d p" "~/.doom.d/packages.el" "doom-packages")
(radian-bookmark "d s" "~/.profile")
(radian-bookmark "d t" "~/.tmux.conf")
(radian-bookmark "d z" "~/.zshrc")

(radian-bookmark "l c" "~/.doom.d/config.local.el" "doom-local-config")
(radian-bookmark "l g" "~/.gitconfig.local")
(radian-bookmark "l i" "~/.doom.d/init.local.el" "doom-local-init")
(radian-bookmark "l p" "~/.doom.d/packages.local.el" "doom-local-packages")
(radian-bookmark "l s" "~/.profile.local")
(radian-bookmark "l t" "~/.tmux.local.conf")
(radian-bookmark "l z" "~/.zshrc.local")

;; This function should exist by default. It doesn't, for some reason.
(defun radian-rename-current-file (newname)
  "Rename file visited by current buffer to NEWNAME.
Interactively, prompt the user for the target filename, with
completion.

If NEWNAME is a directory then extend it with the basename of
`buffer-file-name'. Make parent directories automatically."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "Current buffer is not visiting a file"))
     (let ((newname (read-file-name "Rename to: " nil buffer-file-name)))
       (when (equal (file-truename newname)
                    (file-truename buffer-file-name))
         (user-error "%s" "Can't rename a file to itself"))
       (list newname))))
  (unless buffer-file-name
    (error "Current buffer is not visiting a file"))
  (when (equal (file-truename newname)
               (file-truename buffer-file-name))
    (error "%s: %s" "Can't rename a file to itself" newname))
  (when (equal newname (file-name-as-directory newname))
    (setq newname
          (concat newname (file-name-nondirectory buffer-file-name))))
  (make-directory (file-name-directory newname) 'parents)
  ;; Passing integer as OK-IF-ALREADY-EXISTS means prompt for
  ;; confirmation before overwriting. Why? Who can say...
  (dired-rename-file buffer-file-name newname 0))

(map! ("C-x w" #'radian-rename-current-file))

;;; Interface
;;;; Margins

;; We have no need of line numbers.
(setq display-line-numbers-type nil)

;;;; Minibuffer

;; Reduce the keystroke echo area delay further from 0.02 to
;; effectively instant. Async sucks.
(setq echo-keystrokes 1e-6)

;; Unfortunately, `which-key' sets an internal variable at load time
;; based on the value of `echo-keystrokes', and then later overrides
;; `echo-keystrokes' to the value of this internal variable,
;; effectively overwriting our configuration here. Stop that behavior.
(after! which-key
  (setq which-key-echo-keystrokes echo-keystrokes))

(defadvice! radian--ad-read-passwd-hide-char (func &rest args)
  "Display passwords as **** rather than .... in the minibuffer.
This is the default behavior is Emacs 27, so this advice only has
an effect for Emacs 26 or below."
  :around #'read-passwd
  (let ((read-hide-char (or read-hide-char ?*)))
    (apply func args)))

;; Display minibuffer messages with the prompt color, rather than the
;; default text color. This is for Emacs >26.
(setq minibuffer-message-properties '(face minibuffer-prompt))

;;;; Mode line

;; Use ASCII in the mode line.
(setq doom-modeline-icon nil)

;; Don't show the size of the current selection in progress in the
;; mode line.
(advice-add #'doom-modeline-segment--selection-info :override #'ignore)

;;;; Text appearance

;; Long lines wrap instead of going off the right-hand side of the
;; screen. No horizontal scroll.
(setq-default truncate-lines nil)

;;;; Buffers

;; By default this is bound to `+ivy/switch-workspace-buffer'. We
;; don't use workspaces, however.
(map! ("C-x b" #'ivy-switch-buffer))

;; Doom seems to reset this from its original value.
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;; Windows

;; Bind S-<left>, S-<right>, S-<up>, S-<down> for moving to adjacent
;; windows.
(windmove-default-keybindings)

;; Bind keys for swapping adjacent windows.
(map! ("M-S-<left>" #'buf-move-left
       "M-S-<right>" #'buf-move-right
       "M-S-<up>" #'buf-move-up
       "M-S-<down>" #'buf-move-down))

;; Bind a key for transposing the frame (horizontal to vertical and
;; vice versa). We have to move an existing binding out of the way
;; first.
(map! (:leader :prefix "w"
       "t" #'transpose-frame
       "T" #'persp-temporarily-display-buffer))

;;;; Frames

(add-hook 'window-setup-hook #'toggle-frame-maximized)

;;;; Color theme

(setq doom-theme 'doom-vibrant)

;;; Code intelligence
;;;; Language servers

;; As per <https://emacs-lsp.github.io/lsp-mode/page/performance/>.
(setq read-process-output-max (* 1024 1024))

;;;; Autocompletion

;; By default, while a Company popup is active it steals most of your
;; keyboard inputs. This is not good user experience as it prevents
;; you (for example) from opening a new line by pressing RET. We bind
;; all of those problematic keys back to nil in the
;; `company-active-map', and establish two canonical ways to interact
;; with Company:
;;
;; (1) Accept the first autocompletion by pressing TAB.
;; (2) Accept a different one by pressing M-1 through M-9.
;;
;; If neither of these suffice, you should just type more to narrow
;; the candidates.
(after! company
  (map! (:map company-active-map
         "<down>" nil
         "<return>" nil
         "<up>" nil
         "C-M-s" nil
         "C-SPC" nil
         "C-d" nil
         "C-j" nil
         "C-k" nil
         "C-n" nil
         "C-p" nil
         "C-s" nil
         "C-u" nil
         "M-n" nil
         "M-p" nil
         "RET" nil
         "<tab>" #'company-complete-selection
         "TAB" #'company-complete-selection)))

;; Show quick-reference numbers in the tooltip. (Select a completion
;; with M-1 through M-0.)
(setq company-show-numbers t)

;; Only display 10 candidates (so that any of them can be selected
;; using the above keyboard shortcuts). Setting the second variable to
;; the same value as the first one means that if there isn't enough
;; room below point for 10 candidates to be displayed, then the
;; candidates will be displayed above point instead of being cut off.
(setq company-tooltip-limit 10)
(setq company-tooltip-minimum 10)

(defvar-local radian--company-buffer-modified-counter nil
  "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

(defadvice! radian--ad-company-complete-on-change ()
  "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
  :override #'company--should-begin
  (let ((tick (buffer-chars-modified-tick)))
    (unless (equal tick radian--company-buffer-modified-counter)
      ;; Only trigger completion if previous counter value was
      ;; non-nil (i.e., don't trigger completion just as we're
      ;; jumping to a buffer for the first time).
      (prog1 (and radian--company-buffer-modified-counter
                  (not (and (symbolp this-command)
                            (string-match-p
                             "^\\(company-\\|undo-\\|undo$\\)"
                             (symbol-name this-command)))))
        (setq radian--company-buffer-modified-counter tick)))))

(defadvice! radian--ad-company-update-buffer-modified-counter ()
  "Make sure `radian--company-buffer-modified-counter' is up to date.
If we don't do this on `company--should-continue' as well as
`company--should-begin', then we may end up in a situation where
autocomplete triggers when it shouldn't. Specifically suppose we
delete a char from a symbol, triggering autocompletion, then type
it back, but there is more than one candidate so the menu stays
onscreen. Without this advice, saving the buffer will cause the
menu to disappear and then come back after `company-idle-delay'."
  :after #'company--should-continue
  (setq radian--company-buffer-modified-counter
        (buffer-chars-modified-tick)))

;; Stop Company from echoing function metadata in the echo area, as
;; this conflicts rather badly and nondeterministically with ElDoc.
(after! company
  (setq company-frontends
        (delete #'company-echo-metadata-frontend company-frontends)))

;; Here are two commands that are used to trigger a completion. They
;; use the standard Emacs framework instead of Company, but that
;; doesn't make much sense so we just make them use Company instead.
(map! ([remap completion-at-point] #'company-manual-begin
       [remap complete-symbol] #'company-manual-begin))

(defadvice! radian--ad-company-lsp-setup (&rest _)
  "Disable `company-prescient' sorting by length in some contexts.
Specifically, disable sorting by length if the LSP Company
backend returns fuzzy-matched candidates, which implies that the
backend has already sorted the candidates into a reasonable
order."
  :after #'lsp
  (setq-local company-prescient-sort-length-enable
              (cl-dolist (w lsp--buffer-workspaces)
                (when (thread-first w
                        (lsp--workspace-client)
                        (lsp--client-server-id)
                        (memq '(jsts-ls mspyls bash-ls texlab ts-ls))
                        (not))
                  (cl-return t)))))

;;;; Automatic reformatting

(apheleia-global-mode +1)

(defadvice! radian--ad-save-buffer-reformat-maybe (func &optional arg)
  "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
  :around #'save-buffer
  (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
    (funcall func)))

;;;; Display contextual metadata

(defadvice! radian--ad-eldoc-better-display-message-p (&rest _)
  "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
  :override #'eldoc--message-command-p
  (member (current-message) (list nil eldoc-last-message)))

;;;; Indentation

;; Override misguided default from Doom. Under no circumstances do we
;; want pressing the tab key to insert a literal tab (or bunch of
;; spaces), if we are in a major mode that provides a way to indent
;; the current line.
(setq-default tab-always-indent t)

(defun radian-indent-defun ()
  "Indent the surrounding defun."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beginning (point)))
        (end-of-defun)
        (let ((end (point)))
          (quiet! (indent-region beginning end)))))))

;; By default C-M-q is bound to a strictly less useful "indent
;; following sexp" command, `indent-pp-sexp'.
(map! (:map (emacs-lisp-mode-map lisp-interaction-mode-map)
       "C-M-q" #'radian-indent-defun))

(advice-add #'indent-region :around #'radian-quiet)

;;;; Comments

;; Prevent `newline-and-indent' from continuing the current comment.
;; We can use `indent-new-comment-line' for that when it's desired.
(advice-remove #'newline-and-indent
               #'+default--newline-indent-and-continue-comments-a)

;;; Introspection
;;;; Help

;; Establish binding for `describe-keymap', which otherwise is not
;; accessible.
(map! (:map help-map "M-k" #'describe-keymap))

(define-minor-mode radian-universal-keyboard-quit-mode
  "Minor mode for making C-g work in `helpful-key'."
  :global t
  (if radian-universal-keyboard-quit-mode
      (defadvice! radian--ad-helpful-key-allow-keyboard-quit (&rest _)
        "Make C-g work in `helpful-key'."
        :before #'helpful-key
        ;; The docstring of `add-function' says that if we make our
        ;; advice interactive and the interactive spec is *not* a
        ;; function, then it overrides the original function's
        ;; interactive spec.
        (interactive
         (list
          (let ((ret (read-key-sequence "Press key: ")))
            (when (equal ret "\^G")
              (signal 'quit nil))
            ret))))
    (advice-remove
     #'helpful-key #'radian--ad-helpful-key-allow-keyboard-quit)))

(radian-universal-keyboard-quit-mode +1)

;; Let's establish a standard location for the Emacs source code.
(setq source-directory (expand-file-name "src" user-emacs-directory))

;; This is initialized to nil by `find-func' if the source is not
;; cloned when the library is loaded.
(setq find-function-C-source-directory
      (expand-file-name "src" source-directory))

(defun radian-clone-emacs-source-maybe ()
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))

(defadvice! radian--ad-find-func-clone-emacs-source (&rest _)
  "Clone Emacs source if needed to view definition."
  :before #'find-function-C-source
  (radian-clone-emacs-source-maybe))

(defadvice! radian--ad-helpful-clone-emacs-source (library-name)
  "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
  :before #'helpful--library-path
  (when (member (file-name-extension library-name) '("c" "rs"))
    (radian-clone-emacs-source-maybe)))

(autoload 'company-quickhelp-manual-begin "company-quickhelp" nil 'interactive)

(after! company-quickhelp
  (company-quickhelp-mode +1))

;;; Applications

(map! (:leader :prefix ("a" . "applications")))

;;;; Browser

(map! (:leader :desc "Open URL in browser" "o u" #'browse-url-at-point))

;; For `git-link', link to a particular revision of a file rather than
;; using the branch name in the URL.
(setq git-link-use-commit t)

(use-package! atomic-chrome
  :defer-incrementally t
  :config

  (defvar-local radian-atomic-chrome-url nil
    "The URL of the text area being edited.")

  (defcustom radian-atomic-chrome-setup-hook nil
    "Hook run while setting up an `atomic-chrome' buffer."
    :type 'hook)

  (defadvice! radian--ad-atomic-chrome-setup (url)
    "Save the URL in `radian-atomic-chrome-url'.
Also run `radian-atomic-chrome-setup-hook'."
    :after #'atomic-chrome-set-major-mode
    (setq radian-atomic-chrome-url url)
    (run-hooks 'radian-atomic-chrome-setup-hook))

  ;; Edit in Markdown by default, because many sites support it and
  ;; it's not a big deal if the text area doesn't actually support
  ;; Markdown.
  (setq atomic-chrome-default-major-mode 'markdown-mode)

  (add-hook
   'atomic-chrome-edit-done-hook
   (defun radian--atomic-chrome-switch-back ()
     "Switch back to the browser after finishing with `atomic-chrome'."
     (when-let ((conn (websocket-server-conn
                       (atomic-chrome-get-websocket (current-buffer))))
                (browser
                 (cond
                  ((eq conn atomic-chrome-server-ghost-text)
                   "Firefox")
                  ((eq conn atomic-chrome-server-atomic-chrome)
                   "Chromium")))
                (opener
                 (if IS-MAC
                     "open"
                   "wmctrl")))
       (when (executable-find opener)
         (let ((alt-browser
                (when (eq conn atomic-chrome-server-atomic-chrome)
                  "Google Chrome")))
           (unless (or (zerop (call-process opener nil nil nil "-a" browser))
                       (not alt-browser))
             (call-process opener nil nil nil "-a" alt-browser)))))))

  ;; Listen for requests from the Chrome/Firefox extension.
  (atomic-chrome-start-server))

;;;; Compilation

;; Don't bother saving buffers at all. That's unnecessary. We know to
;; save our buffers if we want them to be updated on disk.
(setq compilation-save-buffers-predicate #'ignore)

(defadvice! radian--ad-compile-pop-to-buffer (buf)
  "Pop to compilation buffer on \\[compile]."
  :filter-return #'compilation-start
  (prog1 buf
    (when-let ((win (get-buffer-window buf)))
      (select-window win))))

;;;; Dired

;; This binding is way nicer than ^. It's inspired by Sunrise
;; Commander.
(map! (:map dired-mode-map
       "J" #'dired-up-directory))

(defadvice! radian--ad-dired-check-for-ls-dired (&rest _)
  "Check if ls --dired is supported ahead of time, and silently.

This advice prevents Dired from printing a message if your ls
does not support the --dired option. (We do this by performing
the check ourselves, and refraining from printing a message in
the problematic case.)"
  :before #'dired-insert-directory
  (when (eq dired-use-ls-dired 'unspecified)
    (setq dired-use-ls-dired
          (eq 0 (call-process insert-directory-program
                              nil nil nil "--dired")))))

(add-hook 'dired-mode-hook #'radian--autorevert-silence)

;; Establish `dired-x' bindings immediately instead of only after
;; Dired has been loaded the first time.
(map! ("C-x C-j" #'dired-jump
       "C-x 4 C-j" #'dired-jump-other-window))

(when IS-MAC
  (defadvice! radian--ad-dired-guess-open-on-macos
    (&rest _)
    :override #'dired-guess-default
    "Cause Dired's '!' command to use open(1).
This advice is only activated on macOS, where it is helpful since
most of the Linux utilities in `dired-guess-shell-alist-default'
are probably not going to be installed."
    "open"))

;;;; Org

;; Prevent Org from overriding the bindings for windmove. By default,
;; these keys are mapped to `org-shiftleft', etc.
(map! (:map org-mode-map
       "S-<left>" nil
       "S-<right>" nil
       "S-<up>" nil
       "S-<down>" nil))

;; Add replacements for the keybindings we just removed. C-<left> and
;; C-<right> are unused by Org. C-<up> and C-<down> are bound to
;; `org-backward-paragraph', etc. (but see below).
(map! (:map org-mode-map
       "C-<left>" #'org-shiftleft
       "C-<right>" #'org-shiftright
       "C-<up>" #'org-shiftup
       "C-<down>" #'org-shiftdown))

(map! (:leader :prefix "a"
       "a" #'org-agenda
       "c" #'org-capture))

;; Show headlines but not content by default.
(setq org-startup-folded 'content)

;; Make C-k smarter with regard to headline tags.
(setq org-special-ctrl-k t)

;; When you create a sparse tree and `org-indent-mode' is enabled, the
;; highlighting destroys the invisibility added by `org-indent-mode'.
;; Therefore, don't highlight when creating a sparse tree.
(setq org-highlight-sparse-tree-matches nil)

;; Prevent Org Agenda from overriding the bindings for
;; windmove.
(map! (:map org-agenda-mode-map
       "S-<up>" nil
       "S-<down>" nil
       "S-<left>" nil
       "S-<right>" nil))

;; Same routine as above. Now for Org Agenda, we could use
;; C-up and C-down because M-{ and M-} are bound to the same
;; commands. But I think it's best to take the same approach
;; as before, for consistency.
(map! (:map org-agenda-mode-map
       "C-<left>" #'org-agenda-do-date-earlier
       "C-<right>" #'org-agenda-do-date-later))

(defadvice! radian--ad-org-agenda-set-default-directory
  (func &rest args)
  "If `org-directory' exists, set `default-directory' to it in the agenda.
This makes the behavior of `find-file' more reasonable."
  :around #'org-agenda
  (let ((default-directory (if (file-exists-p org-directory)
                               org-directory
                             default-directory)))
    (apply func args)))

(defadvice! radian--ad-org-agenda-mode-lighter-simplify (&rest _)
  "Override the `org-agenda' mode lighter to just \"Org-Agenda\"."
  :override #'org-agenda-set-mode-name
  "Org-Agenda")

;; Hide blocked tasks in the agenda view.
(setq org-agenda-dim-blocked-tasks 'invisible)

;; Override customization from Doom. It's unclear to me why we would
;; want the agenda to display three days ago by default. This results
;; in our being unable to see any tasks SCHEDULED for the current day.
(setq org-agenda-start-day nil)

;;;; Terminal emulator

;; Allow usage of C-h and C-x prefixes from within `term'.
(map! (:map term-raw-map
       "C-h" #'help-command
       "C-x" #'Control-X-prefix))

;;;; Version control

;; Add some missing flags to various Magit commands.
(after! magit

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow"))

  (transient-append-suffix 'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;; Allow using `q' to quit out of popups, in addition to `C-g'. See
;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
;; for discussion.
(after! transient
  (transient-bind-q-to-quit))

;; Don't prompt when reverting hunk from `git-gutter'.
(setq git-gutter:ask-p nil)

;;; Shutdown

(defun radian-really-kill-emacs ()
  "Kill Emacs immediately, bypassing `kill-emacs-hook'."
  (interactive)
  (let ((kill-emacs-hook nil))
    (kill-emacs)))

(defun radian-new-emacs (&optional arg)
  "Spawn a new instance of Emacs. See `restart-emacs'."
  (interactive "P")
  (let ((restart-emacs-inhibit-kill-p t))
    (funcall-interactively #'restart-emacs arg)))

(autoload #'restart-emacs--translate-prefix-to-args "restart-emacs")

(defvar radian--restart-in-progress nil
  "Used to prevent infinite recursion.
This is non-nil if `radian--advice-kill-emacs-dispatch' has called
`restart-emacs'.")

(defadvice! radian--ad-kill-emacs-dispatch
  (func &optional arg)
  "Allow restarting Emacs or starting a new session on shutdown."
  :around #'save-buffers-kill-emacs
  (if radian--restart-in-progress
      (funcall func arg)
    (let ((radian--restart-in-progress t)
          ;; Don't mutate the global value.
          (radian--restart-emacs-eager-hook-functions-run nil)
          (prompt (concat "Really exit (or restart, or start new, or kill) "
                          "Emacs? (y/n/r/e/k) "))
          (key nil))
      (while (null key)
        (let ((cursor-in-echo-area t))
          (when minibuffer-auto-raise
            (raise-frame (window-frame (minibuffer-window))))
          (setq key
                (read-key (propertize prompt
                                      'face 'minibuffer-prompt)))
          (pcase key
            ((or ?y ?Y) (funcall func arg))
            ((or ?n ?N))
            ((or ?r ?R)
             (funcall-interactively #'restart-emacs arg))
            ((or ?e ?E)
             (funcall-interactively #'radian-new-emacs arg))
            ((or ?k ?K) (radian-really-kill-emacs))
            (?\C-g (signal 'quit nil))
            (_ (setq key nil)))))
      (message "%s%c" prompt key))))

;; We have our own confirmation function, disable Doom's.
(setq confirm-kill-emacs nil)

;;; Miscellaneous

;; Enable all disabled commands. See
;; <https://www.gnu.org/software/emacs/manual/html_node/elisp/Disabling-Commands.html>.
(setq disabled-command-function nil)

;;; Language support
;;;; Lisp languages

(add-to-list 'safe-local-variable-values
             '(lisp-indent-function . common-lisp-indent-function))

;;;;; Emacs Lisp

(defadvice! radian--ad-elisp-auto-fill-docstrings-correctly (&rest _)
  "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
  :before-until #'fill-context-prefix
  (when (and (derived-mode-p #'emacs-lisp-mode)
             (eq (get-text-property (point) 'face) 'font-lock-doc-face))
    ""))

(defadvice! radian--ad-elisp-eval-buffer-maybe-reload (func &rest args)
  "When evaluating Doom config file, reload Doom instead.
This is typically what I want."
  :around #'+eval/buffer
  (if (and buffer-file-name
           (member (file-truename buffer-file-name)
                   (list
                    (when (bound-and-true-p early-init-file)
                      (file-truename early-init-file))
                    (file-truename user-init-file)
                    (file-truename "~/.doom.d/config.el")
                    (file-truename "~/.doom.d/init.el")
                    (file-truename "~/.doom.d/packages.el")
                    (file-truename "~/.doom.d/config.local.el")
                    (file-truename "~/.doom.d/init.local.el")
                    (file-truename "~/.doom.d/packages.local.el"))))
      (doom/reload)
    (apply func args)))

(map! (:map (emacs-lisp-mode-map lisp-interaction-mode-map)
       "C-c C-k" #'+eval/buffer-or-region))

(defun radian-find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols.
SYMBOL is as in `xref-find-definitions'."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend))
        ;; Make this command behave the same as `find-function' and
        ;; `find-variable', i.e. always prompt for an identifier,
        ;; defaulting to the one at point.
        (xref-prompt-for-identifier t))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

;; By default, C-h f, C-h v, and C-h o are bound to
;; `describe-function', `describe-variable', and `describe-symbol'
;; respectively. By analogy, C-h C-f, C-h C-v, and C-h C-o should be
;; bound as follows. (There's no `find-symbol' function by default for
;; some reason; note that `xref-find-definitions' is not a replacement
;; because it is major-mode dependent.) By further analogy, we should
;; bind `find-library'.
(map! "C-h C-f" #'find-function
      "C-h C-v" #'find-variable
      "C-h C-o" #'radian-find-symbol
      "C-h C-l" #'find-library)

;;;; C, C++, Objective-C, Java

;; Instead of displaying "C++//l" or something similarly
;; incomprehensible in the modeline, just show "C++".
(advice-add #'c-update-modeline :override #'ignore)

;;;; Go

;; The following few forms work around
;; <https://github.com/dominikh/go-mode.el/issues/232>.

(defvar radian--go-defun-regexp
  "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
  "Regexp matching top-level declarations in Go.")

(defun radian--go-beginning-of-defun (&optional arg)
  "Move to beginning of current or previous top-level declaration."
  (cond
   ((null arg)
    (cl-block nil
      (while t
        (re-search-backward radian--go-defun-regexp nil 'noerror)
        (when (or (bobp)
                  (eq (get-text-property (point) 'face)
                      'font-lock-keyword-face))
          (cl-return)))))
   ((> arg 0)
    (dotimes (_ arg)
      (radian--go-beginning-of-defun)))
   ((< arg 0)
    ;; Yuck -- but we need to implement this, otherwise
    ;; `end-of-defun' just does the wrong thing :/
    (dotimes (_ (- arg))
      (radian--go-beginning-of-defun)
      (radian--go-end-of-defun)
      (radian--go-end-of-defun))
    (radian--go-beginning-of-defun))))

(defun radian--go-end-of-defun ()
  "Move to end of current or previous top-level declaration.
Only works if `radian--go-beginning-of-defun' was just called
previously."
  (dotimes (_ 2)
    (cl-block nil
      (while t
        (re-search-forward radian--go-defun-regexp nil 'noerror)
        (when (or (eobp)
                  (save-excursion
                    (beginning-of-line)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face)))
          (cl-return)))))
  (beginning-of-line)
  (go--backward-irrelevant 'stop-at-string)
  (forward-line))

(setq-hook! go-mode
  beginning-of-defun-function #'radian--go-beginning-of-defun
  end-of-defun-function #'radian--go-end-of-defun)

;;;; Markdown

(defun radian-markdown-tab ()
  "Do something reasonable when the user presses TAB.
This means moving forward a table cell, indenting a list item, or
performing normal indentation."
  (interactive)
  (cond
   ((markdown-table-at-point-p)
    (markdown-table-forward-cell))
   ((markdown-list-item-at-point-p)
    (markdown-demote-list-item))
   (t
    ;; Ew. But `markdown-indent-line' checks to see if
    ;; `this-command' is `markdown-cycle' before doing something
    ;; useful, so we have to.
    (let ((this-command 'markdown-cycle))
      (indent-for-tab-command)))))

(defun radian-markdown-shifttab ()
  "Do something reasonable when the user presses S-TAB.
This means moving backward a table cell or unindenting a list
item."
  (interactive)
  (cond
   ((markdown-table-at-point-p)
    (markdown-table-backward-cell))
   ((markdown-list-item-at-point-p)
    (markdown-promote-list-item))))

(map! (:map markdown-mode-map
       "TAB" #'radian-markdown-tab
       "<S-iso-lefttab>" #'radian-markdown-shifttab
       "<S-tab>" #'radian-markdown-shifttab
       "<backtab>" #'radian-markdown-shifttab))

;;;; Python

(defadvice! radian--ad-python-respect-outline-settings (func &rest args)
  "Prevent `python-mode' from overriding `outline-minor-mode' settings.
But only if they were already set by a file-local variable or
something."
  :around #'python-mode
  (flet! ((defun set (symbol newval)
            (unless (and (string-prefix-p
                          "outline-" (symbol-name symbol))
                         (local-variable-p symbol))
              (funcall set symbol newval))))
    (apply func args)))

(add-hook
 'python-mode-hook
 (defun radian--python-no-reindent-on-colon ()
   "Don't reindent (usually wrongly) on typing a colon.
See <https://emacs.stackexchange.com/a/3338/12534>."
   (setq electric-indent-chars (delq ?: electric-indent-chars))))

;;;; Shell

(after! sh-script
  (add-hook
   'sh-mode-hook
   (defun radian--sh-prettify-mode-line ()
     "Instead of \"Shell[bash]\", display mode name as \"Bash\"."
     ;; Only do this for `sh-mode', not derived modes such as
     ;; `pkgbuild-mode'.
     (setq mode-line-process nil)
     (when (eq major-mode 'sh-mode)
       (setq mode-name (capitalize (symbol-name sh-shell)))))
   :append))

;;;; TeX

(advice-add #'TeX-update-style :around #'radian-quiet)
(advice-add #'TeX-auto-list-information :around #'radian-quiet)

;; Use TeXShop for previewing LaTeX, rather than Preview. This means
;; we have to define the command to run TeXShop as a "viewer program",
;; and then tell AUCTeX to use the TeXShop viewer when opening PDFs.
(after! tex
  (when (and IS-MAC
             (or (file-directory-p "/Applications/TeXShop.app")
                 (file-directory-p "/Applications/TeX/TeXShop.app")))
    (add-to-list 'TeX-view-program-list
                 '("TeXShop" "/usr/bin/open -a TeXShop.app %s.pdf"))
    (setf (map-elt TeX-view-program-selection 'output-pdf) '("TeXShop"))))

(defadvice! radian--ad-tex-simplify-mode-name (&rest _)
  "Remove frills from the `mode-name' in TeX modes.
In practice, this means removing the stuff that comes after the
slash, e.g. \"LaTeX/P\" becomes just \"LaTeX\"."
  :after #'TeX-set-mode-name
  (setq mode-name TeX-base-mode-name))

;; Save buffers automatically when compiling, instead of prompting.
(setq TeX-save-query nil)

(defadvice! radian--ad-hide-tex-compilation-buffers (name)
  "Hide AUCTeX compilation buffers by prepending a space to their names.
This prevents them from getting in the way of buffer selection."
  :filter-return #'TeX-process-buffer-name
  (concat " " name))

(put 'LaTeX-using-Biber 'safe-local-variable #'booleanp)

;;;; Web

;; This is also used by `json-mode'. The default is 4, and nobody
;; should indent JSON with four spaces.
(setq js-indent-level 2)

;; Missing `auto-mode-alist' entries for `web-mode'.
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;; Indent by two spaces by default. Compatibility with Prettier.
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;; Insert matching tags automatically. Why this is "mode 2", I have
;; not the slightest idea.
(setq web-mode-auto-close-style 2)

;; Disable `web-mode' automatically reindenting a bunch of surrounding
;; code when you paste anything. It's real annoying if it happens to
;; not know how to indent your code correctly.
(setq web-mode-enable-auto-indentation nil)

;; When using `web-mode' to edit JavaScript files, support JSX tags.
(after! web-mode
  (add-to-list 'web-mode-content-types-alist
               '("jsx" . "\\.js[x]?\\'")))

;; Create line comments instead of block comments by default in
;; JavaScript. See <https://github.com/fxbois/web-mode/issues/619>.
(after! web-mode
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item)
                          (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats))))

(add-hook 'web-mode-hook
  (defun radian--web-js-fix-comments ()
    "Fix comment handling in `web-mode' for JavaScript."
    (when (member web-mode-content-type '("javascript" "jsx"))

      ;; For some reason the default is to insert HTML comments even
      ;; in JavaScript.
      (setq-local comment-start "//")
      (setq-local comment-end "")

      ;; Needed since otherwise the default value generated by
      ;; `comment-normalize-vars' will key off the syntax and think
      ;; that a single "/" starts a comment, which completely borks
      ;; auto-fill.
      (setq-local comment-start-skip "// *"))))

(add-hook 'apheleia-post-format-hook
  (defun radian--web-highlight-after-formatting ()
    "Make sure syntax highlighting works with Apheleia.
The problem is that `web-mode' doesn't do highlighting correctly
in the face of arbitrary buffer modifications, and kind of hacks
around the problem by hardcoding a special case for yanking based
on the value of `this-command'. So, when buffer modifications
happen in an unexpected (to `web-mode') way, we have to manually
poke it. Otherwise the modified text remains unfontified."
    (setq web-mode-fontification-off nil)
    (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
      (save-excursion
        (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end)))))

;;; Local configuration

(load (expand-file-name "config.local.el" doom-private-dir)
      'noerror 'nomessage)

;; Local Variables:
;; fill-column: 70
;; indent-tabs-mode: nil
;; outline-regexp: ";;;+ "
;; sentence-end-double-space: nil
;; End:
