# dotfiles

This repository contains my personal configurations for Zsh, tmux, Leiningen, and Emacs.

# Getting started

If you're on a Mac, simply download or clone this repository and run `scripts/setup.sh` as an executable. Running with no arguments will enable all features (try this for a list—nothing will be installed until you confirm your selection). Alternatively, you can use `include` or `exclude` to enable or disable particular features, e.g. `setup.sh include emacs` or `setup.sh exclude tree tmuxinator`.

If you're on Linux, you will have to move the dotfiles you want into your home directory manually. You can check [`setup.sh`](https://github.com/raxod502/dotfiles/blob/master/scripts/setup.sh) to see what programs are expected to be installed, and what the minimum versions are.

# Emacs

![Emacs vs. Vim cartoon](http://eliot.s3.amazonaws.com/eliotlash.com/0010_en_vi-vs-emacs.png)

## Keyboard shortcut notation

- `f` means press the "F" key.
- `F` means press the "F" key while holding Shift.
- `C-f` means press the "F" key while holding Control.
- `M-f` means press the "F" key while holding Meta. On Mac keyboards, Meta is labeled Alt.
- `M-^` means press the "6" key while holding both Shift and Meta.
- `C-M-f` means press the "F" key while holding both Control and Meta.
- `C-c M-J` means press the "C" key while holding Control, then let go and press the "J" key while holding both Shift and Control.
- `S-left` means press the left arrow key while holding Shift.
- `s-f` means press the "F" key while holding Super. On Mac keyboards, Super is labeled Command.

## Manipulating buffers

### Switch to an already-open buffer

Command | Action
--- | ---
`C-x b` | Switch the buffer of the current window using [`helm`](#helm)

### Switch to a file within a project

Command | Action
--- | ---
`C-c p b` | Switch to a buffer in the current project using [`helm`](#helm)
`C-c p f` | Open a file in the current project using [`helm`](#helm)
`C-c p p` | Open a file in a different project using [`helm`](#helm)
`C-c p h` | Combination of `C-c p b`, `C-c p f`, and `C-c p p`
`C-c p F` | Open a file in any project using [`helm`](#helm) (warning: slow)

The `projectile` package provides some functionality for grouping files into "projects". By default, any folders that are Git repositories are considered projects. Folders will be added automatically to the list of known projects as you visit them.

Any non-file buffers you open while in a Projectile project will be marked as associated with that project, and will show up under `C-c p b` along with any buffers for files in the project.

You can view the project for the current buffer in the mode line at the bottom of Emacs. If you are in the `dotfiles` project, for instance, then your mode line might look something like this:

```
[*] init.el        72% (389,30)  [dotfiles]  (Emacs-Lisp Paredit AggrIndent)
```

### Switch to an arbitrary file, or a new file

Command | Action
--- | ---
`C-x C-f` | Open or create a file

This uses IDO mode.

### Delete a buffer

Command | Action
--- | ---
`C-x k` | Kill a buffer using [`helm`](#helm)

## Manipulating windows

### Switch the buffer of a window

See [Manipulating buffers](#manipulating-buffers).

### Open windows

Command | Action
--- | ---
`C-x 2` | Split the current window horizontally
`C-x 3` | Split the current window vertically

### Move between windows

Command | Action
--- | ---
`S-left` | Move to the window directly to the left of the cursor
`S-right` | Move to the window directly to the right of the cursor
`S-up` | Move to the window directly above the cursor
`S-down` | Move to the window directly below the cursor

Suppose you have a window arrangement like this:

```
+-----------------------+
|                       |
|           A           |
|                       |
+-----------+-----------+
|           |           |
|     B     |     C     |
|           |           |
+-----------+-----------+
```

If you are in `A` and press `S-down`, then whether you move to `B` or to `C` is dependent on which side of `A` the cursor is on when you press `S-down`.

### Close windows

Command | Action
--- | ---
`C-x 0` | Close the current window
`C-x 1` | Close all windows other than the current window

## Moving within a buffer

### Move by characters

Command | Action
--- | ---
`C-b` or `left` | Back one character
`C-f` or `right` | Forward one character
`C-p` or `up` | Up one line
`C-n` or `down` | Down one line

This diagram from the Emacs tutorial may help you remember the first versions of the commands:

```
                          Previous line, C-p
                                  :
                                  :
   Backward, C-b .... Current cursor position .... Forward, C-f
                                  :
                                  :
                            Next line, C-n
```

### Move within a line

Command | Action
--- | ---
`C-a` | Beginning of line
`C-e` | End of line
`M-m` | First non-whitespace character in line

### Move through text

Command | Action
--- | ---
`M-b` | Back one word
`M-f` | Forward one word
`M-a` | Back one sentence
`M-e` | Forward one sentence

In Vim, the concept of a word is very precise, so it is easy to predict exactly where you will end up after performing a move-by-word command. This doesn't appear to be the case in Emacs—the word movement commands are a little hard to predict.

### Move through Lisp code

Command | Action
--- | ---
`C-M-b` | Back one form
`C-M-f` | Forward one form
`C-M-p` | Descend backwards
`C-M-n` | Ascend forwards
`C-M-u` | Ascend backwards
`C-M-d` | Descend forwards
`C-M-a` | Beginning of top-level form
`C-M-e` | End of top-level form

The best way to explain these commands is by example. In the following code snippets, the location of the cursor is shown as a `|`.

#### `C-M-b` (back one form)

```
(reduce + (take-while (partial > 4000000) (filter ev|en? (fibonacci))))
(reduce + (take-while (partial > 4000000) (filter |even? (fibonacci))))
(reduce + (take-while (partial > 4000000) (|filter even? (fibonacci))))
(reduce + (take-while (partial > 4000000) |(filter even? (fibonacci))))
(reduce + (take-while |(partial > 4000000) (filter even? (fibonacci))))
(reduce + (|take-while (partial > 4000000) (filter even? (fibonacci))))
(reduce + |(take-while (partial > 4000000) (filter even? (fibonacci))))
(reduce |+ (take-while (partial > 4000000) (filter even? (fibonacci))))
(|reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
|(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
```

#### `C-M-f` (forward one form)

```
(reduce + (take-while (par|tial > 4000000) (filter even? (fibonacci))))
(reduce + (take-while (partial| > 4000000) (filter even? (fibonacci))))
(reduce + (take-while (partial >| 4000000) (filter even? (fibonacci))))
(reduce + (take-while (partial > 4000000|) (filter even? (fibonacci))))
(reduce + (take-while (partial > 4000000)| (filter even? (fibonacci))))
(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))|))
(reduce + (take-while (partial > 4000000) (filter even? (fibonacci)))|)
(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))|
```

#### `C-M-p` (descend backwards)

Forms for which this command can really shine are not common in Clojure because of the threading macro (`->`). However, for illustrative purposes, here is an example:

```
(assoc (merge n (assoc (assoc m :a 1) :b 2)) :c 3)|
(assoc (merge n (assoc (assoc m :a 1) :b 2)) :c 3|)
(assoc (merge n (assoc (assoc m :a 1) :b 2)|) :c 3)
(assoc (merge n (assoc (assoc m :a 1) :b 2|)) :c 3)
(assoc (merge n (assoc (assoc m :a 1|) :b 2)) :c 3)
```

#### `C-M-n` (ascend forwards)

```
(reduce + (take-while (par|tial > 4000000) (filter even? (fibonacci))))
(reduce + (take-while (partial > 4000000)| (filter even? (fibonacci))))
(reduce + (take-while (partial > 4000000) (filter even? (fibonacci)))|)
(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))|
```

#### `C-M-u` Ascend backwards

```
(reduce + (take-while (partial > 4000000) (filter ev|en? (fibonacci))))
(reduce + (take-while (partial > 4000000) |(filter even? (fibonacci))))
(reduce + |(take-while (partial > 4000000) (filter even? (fibonacci))))
|(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
```

#### `C-M-d` (descend forwards)

```
|(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
(|reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
(reduce + (|take-while (partial > 4000000) (filter even? (fibonacci))))
(reduce + (take-while (|partial > 4000000) (filter even? (fibonacci))))
```

#### `C-M-a` (beginning of top-level form)

```
(when (member 'clojure-mode radon-packages)
  (add-hook 'clojure-mo|de-hook (lambda () (eldoc-mode 1))))


```

```
|(when (member 'clojure-mode radon-packages)
  (add-hook 'clojure-mode-hook (lambda () (eldoc-mode 1))))


```

#### `C-M-e` (end of top-level form)

```
(when (member 'clojure-mode radon-packages)
  (add-hook 'clojure-mo|de-hook (lambda () (eldoc-mode 1))))


```

```
(when (member 'clojure-mode radon-packages)
  (add-hook 'clojure-mode-hook (lambda () (eldoc-mode 1))))
|
```

### Jump to an arbitrary location on-screen

The `ace-jump-mode` package provides a mechanism for jumping to any piece of text you can see on the screen, even if it is in a different window. To use it, pick a word that you would like to jump to. Then press `C-c C-SPC` and enter the first letter of that word. You will notice that the letter you were looking at has been replaced by a different letter. Type this letter to jump to that location.

By adding a `C-u`, you can jump to any character, not just one at the beginning of a word. Or, you can jump to an arbitrary line by adding a second `C-u`. (For this last option, you do not need to provide a character—just press `C-u C-u C-SPC` and type the letter that appears on the line you desire.)

Command | Action
--- | ---
`C-c C-SPC` | Jump to a word
`C-u C-c C-SPC` | Jump to a character
`C-u C-u C-c C-SPC` | Jump to a line

### Move through the entire buffer

Command | Action
--- | ---
`M-<` | Beginning of buffer
`M->` | End of buffer
`M-g g` | Go to line number

## Manipulating text

### Cutting (killing), copying, and pasting (yanking)

Emacs has a bit of a different nomenclature for its editing operations than most other editors. And if you previously used Vim, you will probably be even more confused. Here is a handy table:

Emacs | Every other editor | Vim
--- | --- | ---
Kill | Cut | Kill
Copy | Copy | Yank
Yank | Paste | Paste

Command | Action
--- | ---
`C-SPC` | Mark the beginning of a selection
`C-w` | Kill the selection
`C-k` | Kill the rest of the current line
`M-w` | Copy the selection
`C-y` | Yank the most recently killed/copied text
`M-y` | Cycle back through the history of killed/copied text

Note `C-k`, by default, will not kill the newline at the end of the line unless the line is empty. So, to kill a non-empty line you will have to do `C-a C-k C-k`.

Most editors do not have the feature that Emacs provides with `M-y`, so an explanation is warranted. Whenever you want to yank some text, first press `C-y`. If you hadn't killed or copied anything since [...]

### Miscellaneous

Command | Action
--- | ---
`M-^` | Join line with previous, collapsing whitespace

## Interacting with Elisp code

Command | Action
--- | ---
`C-x C-e` | Evaluate the form before the cursor
`C-M-x` | Evaluate the top-level form surrounding or before the cursor

## Interacting with Clojure code

### Starting a REPL

Command | Action
--- | ---
`C-c M-j` | Start a REPL

If the current file is in a project (that is, you can find a `project.clj` file by looking upwards in the filesystem), then a REPL for that project will be started. Otherwise, a REPL that is not associated with any project will be started. (This functionality is just like running `lein repl`, because that is what `C-c M-j` does.)

### Interacting with the REPL

Command | Action
--- | ---
`C-x C-e` | Evaluate the form before the cursor in the REPL
`C-M-x` | Evaluate the top-level form surrounding or before the cursor in the REPL
`C-c M-n` | Switch the REPL to the namespace defined by the current file

### Using the REPL

Command | Action
--- | ---
`M-p` | Previous command in history
`M-n` | Next command in history
`C-c M-n` | Switch to a namespace using Helm
`C-c C-c` | Interrupt evaluation (this doesn't *always* work, though)
`C-c C-o` | Clear the output of the last command (useful for large outputs that make Emacs slow)
`C-c C-q` | Quit the REPL

Note that, just like Zsh, when you cycle through past commands with `M-p` and `M-n`, if you have already typed part of a command, only past commands that start with the same prefix will be shown. This even works with Paredit mode, so that the matching parentheses will not mess things up.

Don't forget that the REPL is a regular text buffer at its core, so all of Emacs' navigation and editing commands will still work.

## Helm

[...]

# Contributing

If you find any problems or have any ideas for improvements, please do not hesitate to [open an issue](https://github.com/raxod502/dotfiles/issues/new), or—even better—[a pull request](https://github.com/raxod502/dotfiles/compare).

However, without attention to quality, all code becomes unmaintainable. This is especially true for dotfiles, so please try to follow the guidelines in this section when contributing code to this repository. I won't reject contributions just because of a missing period, but it is polite to put in some effort to make it easier for future contributors to maintain your code.

If don't have any problems, but still want to help out, check out [the currently open issues](https://github.com/raxod502/dotfiles/issues)—I can use all the help I can get!

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

I have [an issue tag](https://github.com/raxod502/dotfiles/issues?q=is%3Aissue%20label%3Ahack%20) for hacks, so that they can hopefully be replaced with better solutions someday.

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
