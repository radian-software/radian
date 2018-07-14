;; -*- lexical-binding: t -*-

(require 'map)

(require 'radian-eldoc)
(require 'radian-indent)
(require 'radian-langs)
(require 'radian-patch)
(require 'radian-util)

;; Package `clojure-mode' provides syntax highlighting and indentation
;; for Clojure [1] code.
;;
;; [1]: https://clojure.org/
(use-package clojure-mode
  :init

  ;; We define some patches after `clojure-mode' is loaded. We need to
  ;; make sure `el-patch' knows how to find these patches. Note that
  ;; `el-patch-feature' is not sufficient for this, since
  ;; `radian-clojure-strings-as-docstrings-mode' defines a patch of
  ;; its own when (if) it's enabled.

  (defun radian-el-patch-enable-clojure-mode ()
    "Make all the patches for `clojure-mode' defined."
    (require 'clojure-mode)
    (radian-clojure-strings-as-docstrings-mode +1))

  (defun radian-el-patch-disable-clojure-mode ()
    "Undo the effects of `radian-el-patch-enable-clojure-mode'."
    (radian-clojure-strings-as-docstrings-mode -1))

  (add-hook 'el-patch-pre-validate-hook
            #'radian-el-patch-enable-clojure-mode)
  (add-hook 'el-patch-post-validate-hook
            #'radian-el-patch-disable-clojure-mode)

  :bind (;; Make sure electric indentation *always* works. For some
         ;; reason, if this is omitted, electric indentation works
         ;; most of the time, but it fails inside Clojure docstrings.
         ;; (TAB will add the requisite two spaces, but you shouldn't
         ;; have to do this manually after pressing RET.) I'd like to
         ;; find a more elegant solution to this problem. See [1] for
         ;; the issue tracking this hack, and [2] for a discussion of
         ;; the bug.
         ;;
         ;; [1]: https://github.com/raxod502/radian/issues/2
         ;; [2]: https://github.com/clojure-emacs/clojure-mode/issues/241
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

  (setq clojure-indent-style :align-arguments)

  ;; Ideally, we would be able to set the identation rules for
  ;; *all* keywords at the same time. But until we figure out how
  ;; to do that, we just have to deal with every keyword
  ;; individually. See issue [1].
  ;;
  ;; Avoid using `define-clojure-indent', as `clojure-mode' may not be
  ;; loaded yet and we don't want to load it during eager
  ;; macroexpansion at runtime.
  ;;
  ;; [1]: https://github.com/raxod502/radian/issues/26
  (dolist (spec '((-> 1)
                  (->> 1)
                  (:import 0)
                  (:require 0)
                  (:use 0)))
    (put-clojure-indent (car spec) (cadr spec)))

  ;; `clojure-mode' does not correctly identify the docstrings of
  ;; protocol methods as docstrings, and as such electric and
  ;; aggressive indentation do not work for them. Additionally, when
  ;; you hack a clojure.core function, such as defonce or defrecord,
  ;; to provide docstring functionality, those docstrings are (perhaps
  ;; rightly, but annoyingly) not recognized as docstrings either.
  ;; However, there is an easy way to get electric indentation working
  ;; for all potential docstrings: simply tell `clojure-mode' that
  ;; *all* strings are docstrings. This will not change the font
  ;; locking, because for some weird reason `clojure-mode' determines
  ;; whether you're in a docstring by the font color instead of the
  ;; other way around. Note that this will cause electric indentation
  ;; by two spaces in *all* multiline strings, but since there are not
  ;; very many non-docstring multiline strings in Clojure this is not
  ;; too inconvenient. (And, after all, it's only electric, not
  ;; aggressive, indentation.)
  ;;
  ;; Unfortunately, `clojure-in-docstring-p' is defined as an inline
  ;; function, so we can't override it. Instead, we replace
  ;; `clojure-indent-line'. But inside a new minor mode, so that the
  ;; user can toggle it if they need to use `aggressive-indent-mode'
  ;; and multiline strings that are not docstrings at the same time.
  (define-minor-mode radian-clojure-strings-as-docstrings-mode
    "Treat all Clojure strings as docstrings.
You want to turn this on if you want to treat strings like
docstrings even though they technically are not, and you want to
turn it off if you have multiline strings that are not
docstrings."
    nil nil nil
    (if radian-clojure-strings-as-docstrings-mode
        (progn
          (el-patch-defsubst clojure-in-docstring-p ()
            (el-patch-concat
              "Check whether point is in "
              (el-patch-swap "a docstring" "any type of string")
              ".")
            (let ((ppss (syntax-ppss)))
              ;; are we in a string?
              (when (nth 3 ppss)
                ;; check font lock at the start of the string
                ((el-patch-swap eq memq)
                 (get-text-property (nth 8 ppss) 'face)
                 (el-patch-wrap 1
                   ('font-lock-string-face 'font-lock-doc-face))))))
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

  ;; Improve the performance of `clojure-project-dir' by memoizing it.
  ;; This alleviates some quite horrible lag generated by CIDER
  ;; calling this function continually.

  (defvar radian-clojure-project-dir-cache (make-hash-table :test #'equal)
    "Memoization table for function `radian-clojure-project-root-path'.
The keys are strings naming directories, and the values are
strings naming the corresponding root directories of Clojure
projects.")

  (defun radian-clojure-project-root-path (&optional dir-name)
    "Return the absolute path to the Clojure project's root directory."
    (let ((dir-name (or dir-name default-directory)))
      (unless (map-contains-key radian-clojure-project-dir-cache dir-name)
        (map-put radian-clojure-project-dir-cache
                 dir-name (clojure-project-root-path dir-name)))
      (map-elt radian-clojure-project-dir-cache dir-name)))

  (setq clojure-project-root-function #'radian-clojure-project-root-path))

;; Package `cider' provides integrated Clojure and ClojureScript REPLs
;; directly in Emacs, a Company backend that uses a live REPL
;; connection to retrieve completion candidates, and documentation and
;; source lookups.
(use-package cider
  :config

  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.
  ;;
  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.
  ;;
  ;; Thanks to malabarba on Clojurians Slack for providing the
  ;; following code:

  (defun radian-cider-dump-nrepl-server-log ()
    "Copy contents of *nrepl-server* to beginning of *cider-repl*."
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string)))))

  (add-hook 'cider-connected-hook #'radian-cider-dump-nrepl-server-log)

  ;; Use the :emacs profile defined in profiles.clj. This enables lots
  ;; of cool extra features in the REPL.
  (when (radian-managed-p "~/.lein/profiles.clj")
    (setq cider-lein-parameters "with-profile +emacs repl :headless"))

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
  ;; anything or be prepared for your cursor to suddenly shift buffers
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

  ;; CIDER makes indentation and autocomplete slow, because it gets
  ;; data from the live REPL connection.

  (add-hook 'cider-mode-hook #'radian-slow-indent-mode)
  (add-hook 'cider-repl-mode-hook #'radian-slow-indent-mode)

  (add-hook 'cider-mode-hook #'radian-slow-autocomplete-mode)
  (add-hook 'cider-repl-mode-hook #'radian-slow-autocomplete-mode))

;; Package `clj-refactor' provides automated refactoring commands for
;; Clojure code.
(use-package clj-refactor
  :straight (:host github :repo "raxod502/clj-refactor.el" :branch "fork/3"
                   :upstream (:host github :repo "clojure-emacs/clj-refactor.el"
                                    :branch "master")
                   :files (:defaults "CHANGELOG.md"))
  :init

  (with-eval-after-load 'clojure-mode
    ;; Enable clj-refactor in Clojure buffers. This is adapted from the
    ;; clj-refactor README [1].
    ;;
    ;; [1]: https://github.com/clojure-emacs/clj-refactor.el

    (defun radian-clj-refactor-enable ()
      "Enable `clj-refactor' mode properly.
This means that `yas-minor-mode' also needs to be enabled, and
the `clj-refactor' keybindings need to be installed."
      (clj-refactor-mode +1)
      (yas-minor-mode +1)
      (cljr-add-keybindings-with-prefix "C-c RET"))

    (add-hook 'clojure-mode-hook #'radian-clj-refactor-enable))

  :config

  ;; Make clj-refactor show its messages right away, instead of
  ;; waiting for you to do another command.

  (defalias 'radian-advice-cljr-message-eagerly #'message
    "Make `clj-refactor' show messages right away.
This is an `:override' advice for `cljr--post-command-message'.")

  (advice-add #'cljr--post-command-message :override
              #'radian-advice-cljr-message-eagerly)

  ;; Automatically sort project dependencies after changing them.
  (setq cljr-auto-sort-project-dependencies t)

  ;; Don't print a warning when starting a REPL outside of project
  ;; context.
  (setq cljr-suppress-no-project-warning t)

  :diminish clj-refactor-mode)

(provide 'radian-clojure)
