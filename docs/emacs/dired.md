# Dired

Dired is Emacs' filesystem-management mode. (It stands for "directory
editor".) You can open Dired for the current directory (and also move
the cursor to the current file) using `C-x C-j`. Alternatively, you
can open Dired for any directory by pressing `C-x d` and selecting
one.

Basic usage of Dired is pretty simple. You can move to a file or
directory with the arrow keys, optionally using a prefix argument to
move multiple rows at once. (In Dired, you don't need to hold Meta or
use `C-u` when you send a prefix argument.) Alternatively, you can use
`C-s` to [search] for the filename you want to jump to.

[search]: search.md

To open the current file (or directory), press `RET`. You can open a
file in *view mode* with `v`. This means that the file is not
editable, and you can close it with `q`. It is like a quick-look
feature. As another alternative to `RET`, you can use `o` to open the
file or directory in a new window.

Dired has a lot of commands; you can press `C-h m` to learn about more
of them. Here are the basic ones:

| Command | Operation          |
| ------- | ------------------ |
| `R`     | Move or rename     |
| `C`     | Copy               |
| `D`     | Delete             |
| `M`     | Change permissions |
| `+`     | New directory      |

You can also mark files to perform operations on more than one at a
time. Use `m` to mark the current file or directory, and `u` to unmark
it. You can also use `* s` to mark everything, and `U` to unmark
everything. For more precision, use `* %` to mark files and
directories that match a regular expression.

Emacs makes it harder to delete files; to delete multiple files at
once you need to mark them with `d` instead of `m`.
