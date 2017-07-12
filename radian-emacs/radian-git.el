;;; radian-git.el --- Interfacing with Git

(require 'radian-appearance)
(require 'radian-package)
(require 'radian-patch)

;; Don't show `smerge-mode' in the mode line.
(with-eval-after-load 'smerge-mode
  (diminish 'smerge-mode))

;; A Git Porcelain inside Emacs.
(use-package magit
  :defer-install t
  :bind (;; Add important keybindings for Magit as described in the
         ;; manual [1].
         ;;
         ;; [1]: https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config

  ;; Enable the C-c M-g shortcut to go to a popup of Magit commands
  ;; relevant to the current file.
  (global-magit-file-mode +1))

;; Allows editing Git commit messages from the command line (i.e. with
;; emacs or emacsclient as your core.editor).
(use-package git-commit
  :defer-install t
  :commands (git-commit-setup)
  :init

  ;; Lazy-load `git-commit'.

  (defun radian--enable-git-commit-patches ()
    (require 'git-commit))

  (add-hook 'el-patch-pre-validate-hook #'radian--enable-git-commit-patches)

  (el-patch-defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'")

  (el-patch-defun git-commit-setup-check-buffer ()
    (and buffer-file-name
         (string-match-p git-commit-filename-regexp buffer-file-name)
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
