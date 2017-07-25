;;; radian-find-file.el --- Finding files

(require 'cl-lib)
(require 'radian-bind-key)
(require 'radian-completion)
(require 'radian-custom)
(require 'radian-package)
(require 'radian-patch)

(defcustom radian-find-dotfile-prefix
  (radian-join-keys radian-prefix "e")
  "Prefix key sequence for opening dotfiles.
The function `radian-register-dotfile' creates a keybinding under
this prefix, if you ask it to."
  :group 'radian
  :type 'string)

(defcustom radian-find-dotfile-other-window-prefix
  (radian-join-keys radian-prefix "o")
  "Prefix key sequencing for opening dotfiles in another window.
The function `radian-register-dotfile' creates a keybinding under
this prefix, if you ask it to.")

(defmacro radian-register-dotfile
    (filename &optional keybinding other-window-keybinding)
  "Establish functions and keybindings to open a dotfile.

The FILENAME should be a path relative to the user's home
directory. Two interactive functions are created: one to find the
file in the current window, and one to find it in another window.

If KEYBINDING is non-nil, the first function is bound to that key
sequence after it is prefixed by `radian-find-dotfile-prefix'. If
OTHER-WINDOW-KEYBINDING is nil, the second function is bound to
the same key sequence, but prefixed instead by
`radian-find-dotfile-other-window-prefix' (provided that the two
prefixes are different). If a key sequence (a string) is provided
for OTHER-WINDOW-KEYBINDING, that key sequence is used
instead (provided that the new key sequence is distinct from the
first one). If OTHER-WINDOW-KEYBINDING is neither nil nor a
string, no keybinding is established.

This is best demonstrated by example. Suppose FILENAME is
\".emacs.d/init.el\", KEYBINDING is \"e i\", and
`radian-find-dotfile-prefix' is at its default value of \"M-P
e\", and `radian-find-dotfile-other-window-prefix' is at its
default value of \"M-P o\". Then `radian-register-dotfile' will
create the interactive functions `radian-find-init-el' and
`radian-find-init-el-other-window', and it will bind them to the
key sequences \"M-P e e i\" and \"M-P o e i\" respectively."
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (full-filename (concat "~/" filename))
         (defun-name (intern
                      (replace-regexp-in-string
                       "-+"
                       "-"
                       (concat
                        "radian-find-"
                        (replace-regexp-in-string
                         "[^a-z0-9]" "-"
                         bare-filename)))))
         (defun-other-window-name
           (intern
            (concat (symbol-name defun-name)
                    "-other-window")))
         (docstring (format "Edit file %s."
                            full-filename))
         (docstring-other-window
          (format "Edit file %s, in another window."
                  full-filename))
         (defun-form `(defun ,defun-name ()
                        ,docstring
                        (interactive)
                        (find-file ,full-filename)))
         (defun-other-window-form
           `(defun ,defun-other-window-name ()
              ,docstring-other-window
              (interactive)
              (find-file-other-window ,full-filename)))
         (full-keybinding
          (when keybinding
            (radian-join-keys
             radian-find-dotfile-prefix
             keybinding)))
         (full-other-window-keybinding
          (when (or (null other-window-keybinding)
                    (stringp other-window-keybinding))
            (radian-join-keys
             radian-find-dotfile-other-window-prefix
             (or other-window-keybinding
                 keybinding)))))
    `(progn
       ,defun-form
       ,defun-other-window-form
       (bind-keys
        ,@(when full-keybinding
            `((,full-keybinding . ,defun-name)))
        ,@(when (and full-other-window-keybinding
                     (not (string=
                           full-keybinding
                           full-other-window-keybinding)))
            `((,full-other-window-keybinding
               . ,defun-other-window-name))))
       ;; Return the symbols for the two functions defined.
       (list ',defun-name ',defun-other-window-name))))

;; These keybindings are intended to be mnemonic.

;; Emacs
(radian-register-dotfile ".emacs.d/init.el" "e i")
(radian-register-dotfile ".emacs.d/init.local.el" "e l")

;; Git
(radian-register-dotfile ".gitconfig" "g c")
(radian-register-dotfile ".gitexclude" "g e")
(radian-register-dotfile ".gitconfig.local" "g l")

;; Leiningen
(radian-register-dotfile ".lein/profiles.clj" "l p")

;; Tmux
(radian-register-dotfile ".tmux.conf" "t c")
(radian-register-dotfile ".tmux.local.conf" "t l")

;; Vim
(radian-register-dotfile ".config/nvim/init.vim" "v i")

;; Zsh
(radian-register-dotfile ".zshrc" "z r")
(radian-register-dotfile ".zshrc.before.local" "z b")
(radian-register-dotfile ".zshrc.local" "z l")

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with <M-P e e i> and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable Emacs' built-in version control handling. This improves
;; performance and disables some annoying warning messages and
;; prompts, especially regarding symlinks. See [1].
;;
;; [1]: http://stackoverflow.com/a/6190338/3538165
(setq vc-handled-backends nil)

;; Automatically create any nonexistent parent directories when
;; finding a file. If the buffer for the new file is killed without
;; being saved, then offer to delete the created directory or
;; directories.

(defun radian--advice-find-file-automatically-create-directory
    (original-function filename &rest args)
  "Automatically create and delete parent directories of files.
This is an `:override' advice for `find-file' and friends. It
automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it."
  ;; The variable `dirs-to-delete' is a list of the directories that
  ;; will be automatically created by `make-directory'. We will want
  ;; to offer to delete these directories if the user kills the buffer
  ;; without saving it.
  (let ((dirs-to-delete ()))
    ;; If the file already exists, we don't need to worry about
    ;; creating any directories.
    (unless (file-exists-p filename)
      ;; It's easy to figure out how to invoke `make-directory',
      ;; because it will automatically create all parent directories.
      ;; We just need to ask for the directory immediately containing
      ;; the file to be created.
      (let* ((dir-to-create (file-name-directory filename))
             ;; However, to find the exact set of directories that
             ;; might need to be deleted afterward, we need to iterate
             ;; upward through the directory tree until we find a
             ;; directory that already exists, starting at the
             ;; directory containing the new file.
             (current-dir dir-to-create))
        ;; If the directory containing the new file already exists,
        ;; nothing needs to be created, and therefore nothing needs to
        ;; be destroyed, either.
        (while (not (file-exists-p current-dir))
          ;; Otherwise, we'll add that directory onto the list of
          ;; directories that are going to be created.
          (push current-dir dirs-to-delete)
          ;; Now we iterate upwards one directory. The
          ;; `directory-file-name' function removes the trailing slash
          ;; of the current directory, so that it is viewed as a file,
          ;; and then the `file-name-directory' function returns the
          ;; directory component in that path (which means the parent
          ;; directory).
          (setq current-dir (file-name-directory
                             (directory-file-name current-dir))))
        ;; Only bother trying to create a directory if one does not
        ;; already exist.
        (unless (file-exists-p dir-to-create)
          ;; Make the necessary directory and its parents.
          (make-directory dir-to-create 'parents))))
    ;; Call the original `find-file', now that the directory
    ;; containing the file to found exists. We make sure to preserve
    ;; the return value, so as not to mess up any commands relying on
    ;; it.
    (prog1 (apply original-function filename args)
      ;; If there are directories we want to offer to delete later, we
      ;; have more to do.
      (when dirs-to-delete
        ;; Since we already called `find-file', we're now in the buffer
        ;; for the new file. That means we can transfer the list of
        ;; directories to possibly delete later into a buffer-local
        ;; variable. But we pushed new entries onto the beginning of
        ;; `dirs-to-delete', so now we have to reverse it (in order to
        ;; later offer to delete directories from innermost to
        ;; outermost).
        (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
        ;; Now we add a buffer-local hook to offer to delete those
        ;; directories when the buffer is killed, but only if it's
        ;; appropriate to do so (for instance, only if the directories
        ;; still exist and the file still doesn't exist).
        (add-hook 'kill-buffer-hook
                  #'radian--kill-buffer-delete-directory-if-appropriate
                  'append 'local)
        ;; The above hook removes itself when it is run, but that will
        ;; only happen when the buffer is killed (which might never
        ;; happen). Just for cleanliness, we automatically remove it
        ;; when the buffer is saved. This hook also removes itself when
        ;; run, in addition to removing the above hook.
        (add-hook 'after-save-hook
                  #'radian--remove-kill-buffer-delete-directory-hook
                  'append 'local)))))

;; Add the advice that we just defined.
(advice-add #'find-file :around
            #'radian--advice-find-file-automatically-create-directory)

;; Also enable it for `find-alternate-file' (C-x C-v).
(advice-add #'find-alternate-file :around
            #'radian--advice-find-file-automatically-create-directory)

;; Also enable it for `write-file' (C-x C-w).
(advice-add #'write-file :around
            #'radian--advice-find-file-automatically-create-directory)

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-automatically-create-directory' created
the directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

;; When you open a file, position the cursor at the same place as the
;; last time you edited the file. See [1].
;;
;; [1]: https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Inhibit the message that is usually printed when the `saveplace'
;; file is written.
(el-patch-defun save-place-alist-to-file ()
  (let ((file (expand-file-name save-place-file))
        (coding-system-for-write 'utf-8))
    (with-current-buffer (get-buffer-create " *Saved Places*")
      (delete-region (point-min) (point-max))
      (when save-place-forget-unreadable-files
        (save-place-forget-unreadable-files))
      (insert (format ";;; -*- coding: %s -*-\n"
                      (symbol-name coding-system-for-write)))
      (let ((print-length nil)
            (print-level nil))
        (pp save-place-alist (current-buffer)))
      (let ((version-control
             (cond
              ((null save-place-version-control) nil)
              ((eq 'never save-place-version-control) 'never)
              ((eq 'nospecial save-place-version-control) version-control)
              (t
               t))))
        (condition-case nil
            ;; Don't use write-file; we don't want this buffer to visit it.
            (write-region (point-min) (point-max) file
                          (el-patch-add nil 'nomsg))
          (file-error (message "Saving places: can't write %s" file)))
        (kill-buffer (current-buffer))))))

;; This package introduces a concept of "projects" and provides
;; commands for quick navigation within and between them.
(use-package projectile
  :demand t
  :config

  ;; Enable Projectile everywhere.
  (projectile-mode 1)

  ;; Don't show Projectile in the mode line. (Radian already adds a
  ;; custom indicator for the current project, so there's no need to
  ;; show the mode separately.)
  (setq projectile-mode-line nil))

;; This package provides enhanced versions of the Projectile commands
;; that use Ivy.
(use-package counsel-projectile
  ;; My fork remaps the 'f' action to do a find-file rather than just
  ;; the same as pressing M-o again.
  :recipe (:host github
           :repo "raxod502/counsel-projectile"
           :upstream (:host github
                      :repo "ericdanan/counsel-projectile"))
  :init

  ;; Lazy-load `counsel-projectile'.
  (el-patch-feature counsel-projectile)

  (el-patch-defun counsel-projectile-commander-bindings ()
    (def-projectile-commander-method ?f
      "Find file in project."
      (counsel-projectile-find-file))
    (def-projectile-commander-method ?d
      "Find directory in project."
      (counsel-projectile-find-dir))
    (def-projectile-commander-method ?b
      "Switch to project buffer."
      (counsel-projectile-switch-to-buffer))
    (def-projectile-commander-method ?A
      (el-patch-swap
        "Search project files with ag."
        "Search project files with rg.")
      (el-patch-swap
        (counsel-projectile-ag)
        (counsel-projectile-rg)))
    (def-projectile-commander-method ?s
      "Switch project."
      (counsel-projectile-switch-project)))

  (el-patch-defun counsel-projectile-toggle (toggle)
    "Toggle Ivy version of Projectile commands."
    (if (> toggle 0)
        (progn
          (when (eq projectile-switch-project-action #'projectile-find-file)
            (setq projectile-switch-project-action
                  (el-patch-swap
                    #'counsel-projectile
                    #'counsel-projectile-find-file)))
          (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
          (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
          (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project)
          (define-key projectile-mode-map [remap projectile-ag]
            (el-patch-swap #'counsel-projectile-ag #'counsel-projectile-rg))
          (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
          (counsel-projectile-commander-bindings))
      (progn
        (when (eq projectile-switch-project-action
                  (el-patch-swap
                    #'counsel-projectile
                    #'counsel-projectile-find-file))
          (setq projectile-switch-project-action #'projectile-find-file))
        (define-key projectile-mode-map [remap projectile-find-file] nil)
        (define-key projectile-mode-map [remap projectile-find-dir] nil)
        (define-key projectile-mode-map [remap projectile-switch-project] nil)
        (define-key projectile-mode-map (el-patch-swap
                                          [remap projectile-ag]
                                          [remap projectile-rg])
          nil)
        (define-key projectile-mode-map [remap projectile-switch-to-buffer] nil)
        (projectile-commander-bindings))))

  ;; Enable the `counsel-projectile' keybindings. This does not
  ;; actually load the package, though.
  (with-eval-after-load 'projectile
    (counsel-projectile-toggle 1)))

(use-package ffap
  :ensure nil
  :config

  ;; Don't try to find URLs or remote machines.
  (setq ffap-url-regexp nil)
  (setq ffap-machine-p-local 'reject)
  (setq ffap-machine-p-known 'reject)
  (setq ffap-machine-p-unknown 'reject))

(provide 'radian-find-file)

;;; radian-find-file.el ends here
