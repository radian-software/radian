# Searching and find-replace in Emacs

You can search a single file, or multiple files at once. Emacs also
supports a find-and-replace operation, like many other editors.

## Search a single file

Press `C-s` or `C-r` to search the current buffer. At first, every
line of the file will be shown. However, when you type a query, the
results will be filtered so that only matching lines will be shown.

Navigate through the matches using the arrow keys; when you select a
match, the cursor will move to it in the buffer. You can press `RET`
to dismiss the search dialog and stay at the current match, or `C-g`
to return to the place where you started the search. Either way, you
can use `C-x C-r` to return to the search dialog where you left off.

You can navigate through previous search queries using `M-p` (for
previous) and `M-n` (for next). The keybinding `C-s C-s` is equivalent
to `C-s M-n` and can be used as an abbreviation.

Your query is an Elisp regular expression, with spaces turned into
`.*` tokens. (You can match a literal space with two consecutive
spaces, two literal spaces with three consecutive spaces, and so on.)

## Search multiple files at once

You can use `C-c C-k` to search all files in the current directory and
all of its child directories using [ag].

[ag]: https://github.com/ggreer/the_silver_searcher

Press `RET` to jump to one of the matches. You can then use `C-x C-r`
to jump back to the match list.

You can also search the current [project] using `C-c p s s`, using the
same interface.

[project]: find-files.md

## Find and replace in a single file

Press `M-%` to start a *query replace*. You will be asked to enter a
search string and a target string; both of these can be Python regular
expressions. Once you finish this, you will be presented with each
potential replacement. Note that only matches after the initial
position of the cursor are considered, so if you want to replace in
the entire buffer you should press `M-<` first.

## Find and replace in multiple files

Please check back later. Emacs supports this feature but Radian is not
yet configured to use it, see [issue #207].

[issue #207]: https://github.com/raxod502/radian/issues/207
