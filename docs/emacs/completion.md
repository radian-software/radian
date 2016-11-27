# Autocompletion

Emacs has intelligent autocompletion for many programming languages,
but it will also autocomplete regular text based on the words already
present in the [buffer].

[buffer]: buffers.md

When the completion menu pops up, you can press `TAB` to accept the
first suggestion. Alternatively, you can use any of `M-1` through
`M-0` to accept one of the first ten suggestions. Or, you can use
`M-p` and `M-n` to move through suggestions one at a time. To move
faster, you can page up and down with `C-v` and `M-v`. To kill the
completion menu and make these keys perform their usual functions, use
`C-g`.

As an extra feature, you can also start a search of the current
candidates using `C-s`. Once you have done this, type in your search
query and the first matching candidate will be selected. (Your query
is shown in the mode line at the bottom of the screen.) Type `C-s`
repeatedly to move through matches, and `C-r` to go the opposite
direction. To return to where you were before you started the search,
just press `C-g`. (You will need two `C-g`'s to escape the completion
menu altogether.)

Once you have explicitly interacted with the completion menu in some
way, you can also use the arrow keys instead of `M-p` and `M-n` to
move through suggestions, and `RET` instead of `TAB` to accept a
suggestion.

In some programming languages, the completion menu has additional
functionality. Namely, pressing `C-h` will display documentation for
the current candidate and `C-w` will display the source code for the
current candidate. These functions are not available everywhere.
