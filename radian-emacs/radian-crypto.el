;;; radian-crypto.el --- Applications to cryptography

(require 'radian-patch)

;; Tell `el-patch' about the following patch.
(el-patch-feature epa-file nil)

;; Configuration for EPG, the Emacs frontend to GPG.
(with-eval-after-load 'epa-file

  ;; The function definition we're patching uses one of the setf
  ;; accessors from epg. Emacs' eager macroexpansion will mess this up
  ;; unless we load epg eagerly (when this file is loaded, not after
  ;; `epa-file' is loaded). Thus the use of eval. I know, it's kind of
  ;; horrifying. But it pushes the performance penalty from Emacs init
  ;; time to epg init time.
  (require 'epg)
  (eval
   '(el-patch-defun epa-file-write-region
      (start end file &optional append visit lockname
             mustbenew)
      (if append
          (error "Can't append to the file"))
      (setq file (expand-file-name file))
      (let* ((coding-system (or coding-system-for-write
                                (if (fboundp 'select-safe-coding-system)
                                    ;; This is needed since Emacs 22 has
                                    ;; no-conversion setting for *.gpg in
                                    ;; `auto-coding-alist'.
                                    (let ((buffer-file-name
                                           (file-name-sans-extension file)))
                                      (select-safe-coding-system
                                       (point-min) (point-max)))
                                  buffer-file-coding-system)))
             (context (epg-make-context))
             (coding-system-for-write 'binary)
             string entry
             (recipients
              (cond
               ((listp epa-file-encrypt-to) epa-file-encrypt-to)
               ((stringp epa-file-encrypt-to) (list epa-file-encrypt-to))))
             buffer)
        (epg-context-set-passphrase-callback
         context
         (cons #'epa-file-passphrase-callback-function
               file))
        (epg-context-set-progress-callback
         context
         (cons #'epa-progress-callback-function
               (format "Encrypting %s" file)))
        (setf (epg-context-armor context) epa-armor)
        (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
        (condition-case error
            (setq string
                  (epg-encrypt-string
                   context
                   (if (stringp start)
                       (epa-file--encode-coding-string start coding-system)
                     (unless start
                       (setq start (point-min)
                             end (point-max)))
                     (setq buffer (current-buffer))
                     (with-temp-buffer
                       (insert-buffer-substring buffer start end)
                       ;; Translate the region according to
                       ;; `buffer-file-format', as `write-region' would.
                       ;; We can't simply do `write-region' (into a
                       ;; temporary file) here, since it writes out
                       ;; decrypted contents.
                       (format-encode-buffer (with-current-buffer buffer
                                               buffer-file-format))
                       (epa-file--encode-coding-string (buffer-string)
                                                       coding-system)))
                   (if (or (eq epa-file-select-keys t)
                           (and (null epa-file-select-keys)
                                (el-patch-swap
                                  (not (local-variable-p 'epa-file-encrypt-to
                                                         (current-buffer)))
                                  (null epa-file-encrypt-to))))
                       (epa-select-keys
                        context
                        "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
                        recipients)
                     (if epa-file-encrypt-to
                         (epg-list-keys context recipients)))))
          (error
           (epa-display-error context)
           (if (setq entry (assoc file epa-file-passphrase-alist))
               (setcdr entry nil))
           (signal 'file-error (cons "Opening output file" (cdr error)))))
        (epa-file-run-real-handler
         #'write-region
         (list string nil file append visit lockname mustbenew))
        (if (boundp 'last-coding-system-used)
            (setq last-coding-system-used coding-system))
        (if (eq visit t)
            (progn
              (setq buffer-file-name file)
              (set-visited-file-modtime))
          (if (stringp visit)
              (progn
                (set-visited-file-modtime)
                (setq buffer-file-name visit))))
        (if (or (eq visit t)
                (eq visit nil)
                (stringp visit))
            (message "Wrote %s" buffer-file-name))))))

(provide 'radian-crypto)

;;; radian-crypto.el ends here
