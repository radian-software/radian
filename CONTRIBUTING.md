# Contributing

([back to README](../README.md))

If you find any problems or have any ideas for improvements, please do not hesitate to [open an issue](https://github.com/raxod502/radian/issues/new), or—even better—[a pull request](https://github.com/raxod502/radian/compare).

However, without attention to quality, all code becomes unmaintainable. This is especially true for dotfiles, so please try to follow the guidelines in this document when contributing code to this repository. I won't reject contributions just because of a missing period, but it is polite to put in some effort to make it easier for future contributors to maintain your code.

If you don't have any problems, but still want to help out, check out [the currently open issues](https://github.com/raxod502/radian/issues)—I can use all the help I can get!

## Documentation

> Every line or logical grouping of lines of code in every dotfile **must** have an explanatory comment.

[![Documentation cartoon](http://www.datamation.com/img/2009/12/documentation-please.jpg)](http://www.datamation.com/cnews/article.php/3852701/Tech-Comics-Software-Documentation.htm)

At the very minimum, you must explain what the code is supposed to be doing. This way, people making later modifications can make sure they are not breaking its original functionality.

```
# Open new panes in the same directory as the current pane.
bind % split-window -h -c "#{pane_current_path}"
bind '"' split-window -v -c "#{pane_current_path}"
```

If the line modifies some default setting, you should also make note of what the default was. This makes it easier to tell what will happen if the line is removed.

```
;;; Show completions instantly, rather than after half a second.
(setq company-idle-delay 0)
```

If you are copying or adapting code from somewhere else, please provide any relevant links.

```
;;; Add mouse support
;;; Based on http://stackoverflow.com/a/8859057/3538165
(unless (display-graphic-p)
  (xterm-mouse-mode t)
  ;; Enable scrolling.
  (global-set-key [mouse-4]
                  (lambda ()
                    (interactive)
                    (scroll-down 1)))
  (global-set-key [mouse-5]
                  (lambda ()
                    (interactive)
                    (scroll-up 1))))
```

If it is not immediately obvious why a piece of code is necessary, you **must** explain what problem it solves or what functionality it is intended to change.

```
;;; Sometimes in the CIDER REPL, when Emacs is running slowly, you can
;;; manage to press TAB before the Company completions menu pops
;;; up. This makes a Helm completions buffer appear, which is
;;; disorienting. So we reset TAB to its default functionality
;;; (i.e. indent only) in the CIDER REPL.
(setq cider-repl-tab-command 'indent-for-tab-command)
```

In general, try to find elegant solutions instead of hacks. ("Temporary" hacks have a nasty habit of becoming permanent.) Sometimes, though, there is no alternative. If this is the case, please explain as thoroughly as possible how the hack works and why it is necessary.

I have [an issue tag](https://github.com/raxod502/radian/issues?q=is%3Aissue%20label%3Ahack%20) for hacks, so that they can hopefully be replaced with better solutions someday.

```
           :injections [;; Modify the alembic/distill function to work without needing a
                        ;; project.clj file. This is a hack! It works simply by
                        ;; telling alembic not to look for repositories in the project.clj.
                        ;; However, this is a global override, so if you need to use one
                        ;; of the repositories in the project.clj, you must either pass
                        ;; the :repositories key explicitly, or pass the keyword :project
                        ;; to suppress this hack and allow alembic to search project.clj
                        ;; for repositories.
                        (require 'alembic.still)
                        (alter-var-root
                          #'alembic.still/distill
                          (fn [distill]
                            (fn [dependencies & args]
                              (if (= (first args) :project)
                                (apply distill dependencies (rest args))
                                (apply distill dependencies :repositories
                                       [["central"
                                         {:snapshots false
                                          :url "https://repo1.maven.org/maven2/"}]
                                        ["clojars"
                                         {:url "https://clojars.org/repo/"}]]
                                       args)))))
```

## Commit history

> Try to generate a history that will be as easy as possible for future contributors to read and understand.

[![XKCD: Git Commit](http://imgs.xkcd.com/comics/git_commit.png "Merge branch 'asdfasjkfdlas/alkdjf' into sdkjfls-final")](https://xkcd.com/1296/)

### Commit messages

Follow the generally accepted guidelines for [good Git commit messages](http://chris.beams.io/posts/git-commit/):

> 1. Separate subject from body with a blank line
> 2. Limit the subject line to 50 characters
> 3. Capitalize the subject line
> 4. Do not end the subject line with a period
> 5. Use the imperative mood in the subject line
> 6. Wrap the body at 72 characters
> 7. Use the body to explain what and why vs. how

```
commit 3a6edb77af5e0f220178966f65de9e444d90bfdb
Author: Radon Rosborough <radon.neon@gmail.com>
Date:   Tue Aug 9 22:45:37 2016 -0600

    Don't use source or expand aliases

    Since the .zshrc.aliases system has been removed, these features are no
    longer needed.

    This means we need to add the 'set -e' and 'set -o pipefail' flags on
    every script, once again.
```

When in doubt, explain further. Your future self, and other contributors, will thank you.

### Commit history

Each commit should represent one and only one logical change. Sometimes, it's impractical to separate a refactor into multiple parts, but this is a rare situation.

Instead of sticking a one-line typo fix into your main commit, **make it a separate commit**. There is no such thing as too many commits: more commits means easier bisection and reversion.

However, **do** try to ensure that you don't create intermediate states where things don't compile or are otherwise broken. This makes it harder to bisect.

```
* 5633900 - Replace underscores with dashes in script names (3 days ago) <Radon Rosborough>
* 6554116 - Remove references to unused files (3 days ago) <Radon Rosborough>
* f4c3d67 - Ensure at least version 2.2 of tmux (3 days ago) <Radon Rosborough>
* 7a8f274 - Downcase variable in install_zsh.sh (3 days ago) <Radon Rosborough>
* d815e36 - Remove .zshrc.aliases (3 days ago) <Radon Rosborough>
* 55ff743 - Inject clojure.core/refer-clojure as ./rc (3 days ago) <Radon Rosborough>
* ff8bb54 - Add .gitconfig and corresponding setup.sh support (4 days ago) <Radon Rosborough>
* bba801e - Downcase local variable in install_emacs.sh (4 days ago) <Radon Rosborough>
* 85cfbbd - Suppress 'ls does not support --dired' warning (4 days ago) <Radon Rosborough>
```

## Code style

> Value consistency and common sense over theoretical correctness, but try to follow generally accepted guidelines.

[![XKCD: Code Quality](http://imgs.xkcd.com/comics/code_quality.png)](https://xkcd.com/1513/ "I honestly didn't think you could even USE emoji in variable names. Or that there were so many different crying ones.")

- Comments should end with a period unless they are end-of-line comments spanning only a few words. They should be capitalized, complete sentences.
- Use the Emacs defaults for formatting Elisp. By definition, these are correct.
- Check out [the Clojure style guide](https://github.com/bbatsov/clojure-style-guide). In particular, use `;` for end-of-line comments, `;;` for line comments, `;;;` for top-level line comments, and `;;;;` for section headers.
