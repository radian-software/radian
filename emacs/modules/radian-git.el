;; -*- lexical-binding: t -*-

(require 'radian-appearance)
(require 'radian-patch)

;; Don't show `smerge-mode' in the mode line.
(with-eval-after-load 'smerge-mode
  (diminish 'smerge-mode))

;; Package `git' provides commands which wrap the git(1) command-line
;; tool. It is meant to be used as a library.
(use-package git)

;; Package `with-editor' provides infrastructure for using Emacs as an
;; external editor for programs like Git. It is used by Magit.
(use-package with-editor
  :config/el-patch

  ;; Make sure that `with-editor' always starts a server with a
  ;; nonstandard name, instead of using the default one, so that
  ;; emacsclient from a tty never picks it up (which messes up the
  ;; color theme).
  (defun with-editor--setup ()
    (if (or (not with-editor-emacsclient-executable)
            (file-remote-p default-directory))
        (push (concat with-editor--envvar "=" with-editor-sleeping-editor)
              process-environment)
      ;; Make sure server-use-tcp's value is valid.
      (unless (featurep 'make-network-process '(:family local))
        (setq server-use-tcp t))
      ;; Make sure the server is running.
      (unless (process-live-p server-process)
        (el-patch-splice 2
          (when (server-running-p server-name)
            (setq server-name (format "server%s" (emacs-pid)))
            (when (server-running-p server-name)
              (server-force-delete server-name))))
        (server-start))
      ;; Tell $EDITOR to use the Emacsclient.
      (push (concat with-editor--envvar "="
                    (shell-quote-argument with-editor-emacsclient-executable)
                    ;; Tell the process where the server file is.
                    (and (not server-use-tcp)
                         (concat " --socket-name="
                                 (shell-quote-argument
                                  (expand-file-name server-name
                                                    server-socket-dir)))))
            process-environment)
      (when server-use-tcp
        (push (concat "EMACS_SERVER_FILE="
                      (expand-file-name server-name server-auth-dir))
              process-environment))
      ;; As last resort fallback to the sleeping editor.
      (push (concat "ALTERNATE_EDITOR=" with-editor-sleeping-editor)
            process-environment))))

;; A Git Porcelain inside Emacs.
(use-package magit
  :bind (;; Add important keybindings for Magit as described in the
         ;; manual [1].
         ;;
         ;; [1]: https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (-let (((&alist 'comm comm 'user user)
                                 (process-attributes it)))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       magit-git-executable
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook 'magit-credential-hook
                                #'magit-maybe-start-credential-cache-daemon)))))))

  :config

  ;; Enable the C-c M-g shortcut to go to a popup of Magit commands
  ;; relevant to the current file.
  (global-magit-file-mode +1)

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket))))

;; Package `git-commit' allows you to use Emacsclient as a Git commit
;; message editor, providing syntax highlighting and using
;; `with-editor' to allow you to conveniently accept or abort the
;; commit.
(use-package git-commit
  :init/el-patch

  (defun git-commit-setup-check-buffer ()
    (and buffer-file-name
         (string-match-p git-commit-filename-regexp buffer-file-name)
         (git-commit-setup)))

  (define-minor-mode global-git-commit-mode
    "Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message."
    :group 'git-commit
    :type 'boolean
    :global t
    :init-value t
    :initialize (lambda (symbol exp)
                  (custom-initialize-default symbol exp)
                  (when global-git-commit-mode
                    (add-hook 'find-file-hook 'git-commit-setup-check-buffer)))
    (if global-git-commit-mode
        (add-hook  'find-file-hook 'git-commit-setup-check-buffer)
      (remove-hook 'find-file-hook 'git-commit-setup-check-buffer)))

  :init

  (global-git-commit-mode +1)

  :config

  ;; Wrap summary at 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

(provide 'radian-git)
