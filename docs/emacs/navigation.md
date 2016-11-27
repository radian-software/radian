# Navigation within a file

There are lots of ways of getting around quickly within a file in
Emacs. In fact, there are even more for when you are editing code in a
particular [programming language], but this document only covers the
commands that can be used everywhere.

[programming language]: programming.md

## Basic navigation commands

You can use the arrow keys to move the cursor. Providing a prefix
argument allows you to move by more than one character at a time. If
you dislike the arrow keys for some reason, you can instead use these
alternative keybindings:

                           Previous line, C-p
                                   :
                                   :
    Backward, C-b .... Current cursor position .... Forward, C-f
                                   :
                                   :
                             Next line, C-n

The following commands can be used to move around more quickly:

| Command | Usage                           |
| ------- | ------------------------------- |
| `C-a`   | Go to beginning of current line |
| `C-e`   | Go to end of current line       |
| `M-b`   | Go back one word                |
| `M-f`   | Go forward one word             |
| `M-a`   | Go back one sentence            |
| `M-e`   | Go forward one sentence         |
| `C-v`   | Scroll down one screenful       |
| `M-v`   | Scroll up one screenful         |

After scrolling, you might find that the cursor is near the beginning
or end of the buffer. In this case you can use `C-l` to recenter the
window so that the cursor is in the center. Press `C-l` more times to
recenter the cursor at the top and bottom of the window.

## Go to line

Use `M-g M-g` to go to a particular line in the file, by number.

## Jump anywhere onscreen

There are a number of keybindings you can use to jump to arbitrary
places onscreen. When you press one of these keybindings, highlighted
letters will appear onscreen. Simply type the ones at the location you
want to jump to, and the cursor will move there.

| Command             | Usage                                   |
| ------------------- | --------------------------------------- |
| `M-RET g c <char>`  | jump to occurrences of `<char>`         |
| `M-RET g t <chars>` | jump to occurrences of string `<chars>` |
| `M-RET g l`         | jump to lines                           |
| `M-RET g w`         | jump to words                           |
| `M-RET g W <char>`  | jump to words beginning with `<char>`   |

## Jumping around

Many commands, such as `M-<` and `C-s` among many others, will record
your cursor's position before moving it. (You can also manually tell
Emacs to remember the location of your cursor using `C-SPC C-SPC`.)
You can make the cursor jump back to a previous position with `C-u
C-SPC`, and you can undo the effects of that command using `M--
C-SPC`.

Those commands only apply to the cursor's movements within a single
buffers. Each buffer has its own history of cursor positions, but
there is also a global history of cursor positions that is updated
every time the cursor position is saved in a new buffer. You can jump
backwards in that history using `C-x C-SPC`, and you can undo the
effects of that command using `C-u C-x C-SPC`.
