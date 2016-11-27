# Text editing

As in all text editors, you can insert text at the cursor simply by
typing, and you can delete text before the cursor using `DEL`. In
Emacs, you can also delete text after the cursor using `C-d`.

## Copy and paste

Emacs refers to these operations by somewhat more violent names than
most other editors. For users coming from Vim, the situation is even
worse because Vim has its own terminology. Here is a handy table
correlating everything:

| Normal name | Emacs name | Vim name |
| ----------- | ---------- | -------- |
| Copy        | Copy       | Yank     |
| Cut         | Kill       | Delete   |
| Paste       | Yank       | Paste    |

You can select text by pressing `C-SPC` and then moving the cursor.
From there, you can copy it with `M-w` or kill it with `C-w`. You can
also move the cursor to the other end of the selection with `C-x C-x`.

Selecting the entire buffer can be done with `C-x h` or `C-x C-p`.
(Technically, `C-x C-p` selects the current *page*, but people rarely
use pages anymore.)

To yank the most recent kill (or copy), press `C-y`. However, since
Emacs saves all of your kills, you can also press `M-y` to
interactively select which one you want to yank.

## Shortcuts for killing text

You can use `M-DEL` to kill to the beginning of the current word,
`M-d` to kill the rest of the current word, `C-k` to kill the rest of
the current line, and `M-k` to kill the rest of the current sentence.

## Dealing with multiple lines

You can tell Emacs to automatically wrap your paragraphs at 70
characters with `M-x auto-fill-mode`. (This value can be changed with
`C-x f`.) Alternatively, you can wrap just a single paragraph by
pressing `M-q`.

To join the current line with the previous one (and remove extra
whitespace), use `M-^`. To instead join it with the next one, use `C-u
M-^`.

The whitespace-collapsing function of the join-line commands can also
be performed on its own using `M-SPC`.

## Advanced text manipulation

Use `M-x overwrite-mode` to type over other text rather than inserting
new text.
