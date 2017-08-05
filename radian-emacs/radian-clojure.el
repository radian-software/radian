;;; radian-clojure.el --- Support for Clojure(Script)

(require 'radian-autocomplete)
(require 'radian-eldoc)
(require 'radian-indent)
(require 'radian-lisp)
(require 'radian-package)
(require 'radian-patch)

;; Provides indentation and syntax highlighting for Clojure code.
(use-package clojure-mode
  :defer-install t
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  :init

  ;; We define some patches after clojure-mode is loaded. We need to
  ;; make sure el-patch knows how to find these patches.

  (defun radian--enable-clojure-mode-patches ()
    (require 'clojure-mode)
    (radian-clojure-strings-as-docstrings-mode 1))

  (defun radian--disable-clojure-mode-patches ()
    (radian-clojure-strings-as-docstrings-mode -1))

  (add-hook 'el-patch-pre-validate-hook
            #'radian--enable-clojure-mode-patches)
  (add-hook 'el-patch-post-validate-hook
            #'radian--disable-clojure-mode-patches)

  :bind (;; Make sure electric indentation *always* works. For some
         ;; reason, if this is omitted, electric indentation works most
         ;; of the time, but it fails inside Clojure docstrings. (TAB
         ;; will add the requisite two spaces, but you shouldn't have to
         ;; do this manually after pressing RET.) I'd like to find a more
         ;; elegant solution to this problem. See [1].
         ;;
         ;; <return> is for windowed Emacs; RET is for terminal Emacs.
         ;;
         ;; [1]: https://github.com/raxod502/radian/issues/2
         :map clojure-mode-map
         ("<return>" . newline-and-indent)
         ("RET" . newline-and-indent))
  :config

  ;; Enable Aggressive Indent in Clojure mode.
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

  ;;; Customize indentation like this:
  ;;;
  ;;; (some-function
  ;;;   argument
  ;;;   argument)
  ;;;
  ;;; (some-function argument
  ;;;                argument)
  ;;;
  ;;; (-> foo
  ;;;   thread
  ;;;   thread)
  ;;;
  ;;; (->> foo
  ;;;   thread
  ;;;   thread)
  ;;;
  ;;; (:keyword
  ;;;   map)

  (setq clojure-indent-style ':align-arguments)

  ;; We can't use define-clojure-indent here, due to a perverse
  ;; threefold conspiracy perpetrated by dash.el, recursive
  ;; macroexpansion, and the Gilardi scenario. See [1].
  ;;
  ;; Ideally, we would be able to set the identation rules for
  ;; *all* keywords at the same time. But until we figure out how
  ;; to do that, we just have to deal with every keyword
  ;; individually. See issue [2].
  ;;
  ;; [1]: http://emacs.stackexchange.com/q/26261/12534
  ;; [2]: https://github.com/raxod502/radian/issues/26
  (dolist (spec '((-> 1)
                  (->> 1)
                  (:import 0)
                  (:require 0)
                  (:use 0)))
    (put-clojure-indent (car spec) (cdr spec)))

  ;; `clojure-mode' does not correctly identify the docstrings of
  ;; protocol methods as docstrings, and as such electric and
  ;; aggressive indentation do not work for them. Additionally, when
  ;; you hack a clojure.core function, such as defonce or defrecord,
  ;; to provide docstring functionality, those docstrings are
  ;; (perhaps rightly, but annoyingly) not recognized as docstrings
  ;; either. However, there is an easy way to get electric indentation
  ;; working for all potential docstrings: simply tell `clojure-mode'
  ;; that *all* strings are docstrings. This will not change the font
  ;; locking, because for some weird reason `clojure-mode' determines
  ;; whether you're in a docstring by the font color instead of the
  ;; other way around. Note that this will cause electric indentation
  ;; by two spaces in *all* multiline strings, but since there are not
  ;; very many non-docstring multiline strings in Clojure this is not
  ;; too inconvenient. (And, after all, it's only electric, not
  ;; aggressive, indentation.)

  ;; Unfortunately, `clojure-in-docstring-p' is defined as an inline
  ;; function, so we can't override it. Instead, we replace
  ;; `clojure-indent-line'. But inside a new minor mode, so that the
  ;; user can toggle it if they need to use `aggressive-indent-mode'
  ;; and multiline strings that are not docstrings at the same time.

  (define-minor-mode radian-clojure-strings-as-docstrings-mode
    "Treat all Clojure strings as docstrings.
You want to turn this off if you have multiline strings that are
not docstrings."
    nil nil nil
    (if radian-clojure-strings-as-docstrings-mode
        (progn
          (el-patch-defsubst clojure-in-docstring-p ()
            "Check whether point is in a docstring."
            (el-patch-wrap 1 1
              (or
               (eq (get-text-property (point) 'face) 'font-lock-doc-face)
               (eq (get-text-property (point) 'face) 'font-lock-string-face))))
          (el-patch-defun clojure-indent-line ()
            "Indent current line as Clojure code."
            (if (clojure-in-docstring-p)
                (save-excursion
                  (beginning-of-line)
                  (when (and (looking-at "^\\s-*")
                             (<= (string-width (match-string-no-properties 0))
                                 (string-width (clojure-docstring-fill-prefix))))
                    (replace-match (clojure-docstring-fill-prefix))))
              (lisp-indent-line))))
      (el-patch-unpatch 'clojure-in-docstring-p 'defsubst)
      (el-patch-unpatch 'clojure-indent-line 'defun)))

  (add-hook 'clojure-mode-hook #'radian-clojure-strings-as-docstrings-mode)

  ;; Improve the performance of `clojure-project-dir' by memoizing it.
  ;; This alleviates some quite horrible lag generated by CIDER
  ;; calling this function continually.

  (defvar clojure-project-dir-cache (make-hash-table :test 'equal))

  (el-patch-defun clojure-project-dir (&optional dir-name)
    "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
    (el-patch-let (($dir-name (dir-name (or dir-name default-directory)))
                   ($choices (choices (delq nil
                                            (mapcar (lambda (fname)
                                                      (locate-dominating-file dir-name fname))
                                                    clojure-build-tool-files))))
                   ($body (when (> (length choices) 0)
                            (car (sort choices #'file-in-directory-p)))))
      (el-patch-swap
        (let* ($dir-name
               $choices)
          $body)
        (let ($dir-name)
          (or (gethash dir-name clojure-project-dir-cache)
              (puthash dir-name
                       (let ($choices) $body)
                       clojure-project-dir-cache)))))))

;; Provides Clojure and ClojureScript REPL integration, including
;; documentation and source lookups, among many other features.
(use-package cider
  :defer-install t
  :init

  ;; We define some patches after CIDER is loaded. We need to make
  ;; sure el-patch knows how to find these patches.
  (el-patch-feature cider)

  :bind (;; Allow usage of the C-c M-j and C-c M-J shortcuts everywhere.
         ("C-c M-j" . cider-jack-in)
         ("C-c M-J" . cider-jack-in-clojurescript))
  :config

  ;; Enable ElDoc in Clojure files when CIDER is running.
  (add-hook 'cider-mode-hook #'eldoc-mode)

  ;; Enable ElDoc in the CIDER REPL.
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.

  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.

  ;; Thanks to malabarba on Clojurians Slack for providing the
  ;; following code:

  (defun radian--dump-nrepl-server-log ()
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string)))))

  (add-hook 'cider-connected-hook #'radian--dump-nrepl-server-log)

  ;; Use the :emacs profile defined in profiles.clj. This enables lots
  ;; of cool extra features in the REPL.
  (setq cider-lein-parameters "with-profile +emacs repl :headless")

  ;; The CIDER welcome message often obscures any error messages that
  ;; the above code is supposed to be making visible. So, we need to
  ;; turn off the welcome message.
  (setq cider-repl-display-help-banner nil)

  ;; Sometimes in the CIDER REPL, when Emacs is running slowly, you
  ;; can manage to press TAB before the Company completions menu pops
  ;; up. This triggers a `completing-read', which is disorienting. So
  ;; we reset TAB to its default functionality (i.e. indent only) in
  ;; the CIDER REPL.
  (setq cider-repl-tab-command 'indent-for-tab-command)

  ;; Don't focus the cursor in the CIDER REPL once it starts. Since
  ;; the REPL takes so long to start up, especially for large
  ;; projects, you either have to wait for a minute without doing
  ;; anything or be prepared for your cursur to suddenly shift buffers
  ;; without warning sometime in the near future. This is annoying, so
  ;; turn off the behavior. For a historical perspective see [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/cider/issues/1872
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)

  ;; Use figwheel-sidecar for launching ClojureScript REPLs. This
  ;; supports a fully integrated ClojureScript development experience
  ;; in Emacs, for use with e.g. [1]. The last three forms are from
  ;; the definition of `cider--cljs-repl-types'; the first two work
  ;; around [2].
  ;;
  ;; [1]: https://github.com/reagent-project/reagent-template
  ;; [2]: https://github.com/reagent-project/reagent-template/issues/132
  (setq cider-cljs-lein-repl
        "(do
  (require 'clojure.java.shell)
  (clojure.java.shell/sh \"lein\" \"clean\")
  (require 'figwheel-sidecar.repl-api)
  (figwheel-sidecar.repl-api/start-figwheel!)
  (figwheel-sidecar.repl-api/cljs-repl))")

  ;; Don't show CIDER in the mode line.
  (setq cider-mode-line nil)

  ;; Make some modes perform their actions less often in buffers where
  ;; CIDER is active. This is helpful because CIDER is slow, and will
  ;; lag Emacs if its logic is called too often.

  (defun radian--disable-aggressive-indent-on-save ()
    (remove-hook 'before-save-hook
                 ;; Yes, this is a typo in `aggressive-indent'.
                 #'aggressive-indent--proccess-changed-list-and-indent
                 'local))

  (defun radian--reduce-cider-lag ()
    (setq-local company-idle-delay 1) ; increased from 0
    (setq-local company-minimum-prefix-length 3) ; increased from 0
    (setq-local eldoc-idle-delay 0.5) ; increased from 0
    (when cider-mode
      (radian--disable-aggressive-indent-on-save)
      (add-hook 'aggressive-indent-mode-hook
                #'radian--disable-aggressive-indent-on-save)))

  (add-hook 'cider-mode-hook #'radian--reduce-cider-lag)
  (add-hook 'cider-repl-mode-hook #'radian--reduce-cider-lag)

  ;; Suppress the "Starting a custom ClojureScript REPL" message,
  ;; because it provides no useful information.

  (el-patch-defun cider-create-sibling-cljs-repl (client-buffer)
    "Create a ClojureScript REPL with the same server as CLIENT-BUFFER.
The new buffer will correspond to the same project as CLIENT-BUFFER, which
should be the regular Clojure REPL started by the server process filter."
    (interactive (list (cider-current-connection)))
    ;; Load variables in .dir-locals.el into the server process buffer, so
    ;; cider-cljs-*-repl can be set for each project individually.
    (hack-local-variables)
    (let* ((nrepl-repl-buffer-name-template "*cider-repl CLJS%s*")
           (nrepl-create-client-buffer-function #'cider-repl-create)
           (nrepl-use-this-as-repl-buffer 'new)
           (client-process-args (with-current-buffer client-buffer
                                  (unless (or nrepl-server-buffer nrepl-endpoint)
                                    (error "This is not a REPL buffer, is there a REPL active?"))
                                  (list (car nrepl-endpoint)
                                        (elt nrepl-endpoint 1)
                                        (when (buffer-live-p nrepl-server-buffer)
                                          (get-buffer-process nrepl-server-buffer)))))
           (cljs-proc (apply #'nrepl-start-client-process client-process-args))
           (cljs-buffer (process-buffer cljs-proc))
           (cljs-repl-form (cider-cljs-repl-form (cider-project-type))))
      (with-current-buffer cljs-buffer
        ;; The new connection has now been bumped to the top, but it's still a
        ;; Clojure REPL!  Additionally, some ClojureScript REPLs can actually take
        ;; a while to start (some even depend on the user opening a browser).
        ;; Meanwhile, this REPL will gladly receive requests in place of the
        ;; original Clojure REPL.  Our solution is to bump the original REPL back
        ;; up the list, so it takes priority on Clojure requests.
        (cider-make-connection-default client-buffer)
        (el-patch-remove
          (pcase (assoc cljs-repl-form cider--cljs-repl-types)
            (`(,_ ,name ,info)
             (message "Starting a %s REPL%s" name (or info "")))
            (_ (message "Starting a custom ClojureScript REPL"))))
        (cider-nrepl-send-request
         `("op" "eval"
           "ns" ,(cider-current-ns)
           "code" ,cljs-repl-form)
         (cider-repl-handler (current-buffer)))
        (when cider-offer-to-open-cljs-app-in-browser
          (cider--offer-to-open-app-in-browser nrepl-server-buffer))))))

;; Makes Emacs into a real Clojure IDE by providing a mountain of
;; automated refactoring tools.
(use-package clj-refactor
  ;; Waiting on https://github.com/clojure-emacs/clj-refactor.el/pull/385
  :recipe (:host github :repo "raxod502/clj-refactor.el"
           :upstream (:host github :repo "clojure-emacs/clj-refactor.el")
           :files (:defaults "CHANGELOG.md"))
  :defer-install t
  :init

  ;; By default, clj-refactor enables the refactor-nrepl middleware in
  ;; an *autoload*, meaning that it's already happened as soon as
  ;; clj-refactor has loaded. But refactor-nrepl doesn't work outside
  ;; a Clojure project, and signals a warning in that case. So we need
  ;; to *selectively* inject refactor-nrepl, which means we need to
  ;; disable it right away so we can do it buffer-locally later. See
  ;; [1] for discussion of the problem.
  ;;
  ;; [1]: https://github.com/clojure-emacs/refactor-nrepl/issues/177
  (setq cljr-inject-dependencies-at-jack-in nil)

  (with-eval-after-load 'clojure-mode
    ;; Enable clj-refactor in Clojure buffers. This is adapted from the
    ;; clj-refactor README [1].
    ;;
    ;; [1]: https://github.com/clojure-emacs/clj-refactor.el

    (defun radian--enable-clj-refactor-mode ()
      "Enable `clj-refactor' mode properly.
This means that `yas-minor-mode' also needs to be enabled, and
the `clj-refactor' keybindings need to be installed."
      (when (and (use-package-install-deferred-package 'clj-refactor :after)
                 (use-package-install-deferred-package 'yasnippet :after))
        (clj-refactor-mode 1)
        (yas-minor-mode 1)
        (cljr-add-keybindings-with-prefix "C-c RET")))

    (add-hook 'clojure-mode-hook #'radian--enable-clj-refactor-mode))

  (with-eval-after-load 'cider
    ;; Because we disabled injection of the clj-refactor middleware
    ;; earlier, we have to do it buffer-locally now (but only if we're
    ;; in a project).

    (defun radian--advice-inject-cljr-dependencies (&rest args)
      "Re-enable injection of `clj-refactor' middleware.
This is a `:before' advice for `cider-jack-in'."
      (when (and (use-package-install-deferred-package
                  'clj-refactor :after)
                 (cljr--project-dir))
        (setq-local cljr-inject-dependencies-at-jack-in t)
        (make-local-variable 'cider-jack-in-lein-plugins)
        (make-local-variable 'cider-jack-in-nrepl-middlewares)
        (cljr--inject-jack-in-dependencies)))

    (advice-add #'cider-jack-in :before
                #'radian--advice-inject-cljr-dependencies))

  :config

  ;; We also need to tell clj-refactor not to check that
  ;; refactor-nrepl is installed properly when we are not in a
  ;; project.

  (defalias 'radian--advice-cljr-suppress-middleware-check #'cljr--project-dir
    "Suppress a spurious warning from `clj-refactor'.
This is a `:before-while' advice for `cljr--init-middleware'. The
warning in question is printed in the REPL and tells you that the
middleware is not injected, even though it shouldn't be injected
when you're outside a project.")

  (advice-add #'cljr--init-middleware :before-while
              #'radian--advice-cljr-suppress-middleware-check)

  ;; Make clj-refactor show its messages right away, instead of
  ;; waiting for you to do another command.

  (defalias 'radian--advice-cljr-message-eagerly #'message
    "Make `clj-refactor' show messages right away.
This is an `:override' advice for `cljr--post-command-message'.")

  (advice-add #'cljr--post-command-message :override
              #'radian--advice-cljr-message-eagerly)

  ;; Automatically sort project dependencies after changing them.
  (setq cljr-auto-sort-project-dependencies t)

  :diminish clj-refactor-mode)

(provide 'radian-clojure)

;;; radian-clojure.el ends here
