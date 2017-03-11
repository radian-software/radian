;;; radian-git.el --- Interfacing with Git

(require 'radian-package)
(require 'radian-patch)

;; Don't show `smerge-mode' in the mode line.
(with-eval-after-load 'smerge-mode
  (diminish 'smerge-mode))

;; A Git Porcelain inside Emacs.
(use-package magit
  :recipe (:fetcher github
           :repo "raxod502/magit"
           :branch "magit-version-symlinks"
           :files ("lisp/magit*.el"
                   "lisp/git-rebase.el"
                   "Documentation/magit.texi"
                   "Documentation/AUTHORS.md"
                   "COPYING" (:exclude "lisp/magit-popup.el")))
  :bind (;; Add important keybindings for Magit as described in the
         ;; manual [1].
         ;;
         ;; [1]: https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; Allows editing Git commit messages from the command line (i.e. with
;; emacs or emacsclient as your core.editor).
(use-package git-commit
  :init

  ;; Lazy-load `git-commit'.

  (el-patch-defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'")

  (el-patch-defun git-commit-setup-check-buffer ()
    (and buffer-file-name
         (string-match-p git-commit-filename-regexp buffer-file-name)
         ;; The `git-commit-setup' function is not autoloaded, so we have
         ;; to `require' the package manually.
         (el-patch-add
           (require 'git-commit))
         (git-commit-setup)))

  (el-patch-define-minor-mode global-git-commit-mode
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

  (global-git-commit-mode 1)

  :config

  ;; Wrap summary at 50 characters as per [1].
  ;;
  ;; [1]: http://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50))

(provide 'radian-git)

;;; radian-git.el ends here
