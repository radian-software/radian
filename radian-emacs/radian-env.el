;;; radian-env.el --- Environment variables

(require 'radian-util)

(defcustom radian-env-profile-file "~/.profile"
  "Name of file sourced to get environment variables, or nil.
If nil, then sourcing doesn't happen."
  :group 'radian
  :type '(choice (const nil) string))

(defcustom radian-env-output-buffer-name "*radian-env-output*"
  "Name of buffer used for sourcing ~/.profile."
  :group 'radian
  :type 'string)

(when (and radian-env-profile-file
           (file-exists-p radian-env-profile-file)
           (executable-find "python"))
  (ignore-errors (kill-buffer radian-env-output-buffer-name))
  (with-current-buffer (get-buffer-create radian-env-output-buffer-name)
    (let* ((python-script
            (expand-file-name "scripts/print_env.py" radian-directory))
           (delimiter (radian-random-string))
           (sh-script (format ". %s && %s %s"
                              (shell-quote-argument
                               (expand-file-name radian-env-profile-file))
                              (shell-quote-argument python-script)
                              (shell-quote-argument delimiter)))
           (return (call-process "sh" nil t nil "-c" sh-script))
           (found-delimiter
            (progn
              (goto-char (point-min))
              (search-forward delimiter nil 'noerror))))
      (if (and (= 0 return) found-delimiter)
          (let* ((results (split-string
                           (buffer-string) (regexp-quote delimiter)))
                 (results (cl-subseq results 1 (1- (length results)))))
            (if (evenp (length results))
                (cl-loop for (var value) on results by #'cddr do
                         (setenv var value))
              (message "Loading %s produced malformed result; see %s"
                       radian-env-profile-file
                       radian-env-output-buffer-name)))
        (message "Failed to load %s; see %s"
                 radian-env-profile-file
                 radian-env-output-buffer-name)))))

(provide 'radian-env)
