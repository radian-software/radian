# Emacs fundamentals

Here we cover:

* the notation for keyboard shortcuts
* how to run commands
* how to provide arguments to commands
* selecting from lists of options

First we talk about keyboard shortcuts, then commands that don't have
keyboard shortcuts, and finally how to make commands perform
differently.

## Keyboard shortcuts

Emacs has a precise notation for keyboard shortcuts because there are
a lot of them and it would be very verbose to keep saying things like
"and then press J while holding Shift and Alt".

The best way to explain this notation is probably by example. Here are
the basic pieces:

* `f` means press the F key.
* `F` means you should also hold Shift.
* `C-f` means you should hold Control.
* `M-f` means you should hold Meta. On most keyboards, this key is
  called Alt. You may have to set the settings of your terminal
  emulator so that pressing Alt sends Meta to Emacs. If you can't get
  Meta to work, you can also send `M-f` by pressing Escape and then F,
  sequentially.
* `s-f` means you should hold Super. On Apple keyboards, this key is
  called Command; on Windows keyboards, it is the Windows key. Very
  few Emacs commands use Super because typically that key is used for
  keyboard shortcuts by the operating system.

These pieces can all be combined:

* `C-M-F` means you should press F while holding Shift, Control, and
  Meta at the same time.
* `C-c M-J` means you should press C while holding Control, and then
  press J while holding Shift and Meta.

Special keys like `TAB` or `<tab>` are represented just like that. If
you need to hold Shift while pressing a special key, that's denoted
like `<S-left>`. (Don't confuse this with `<s-left>`, which means the
Super key!)

## Commands without keyboard shortcuts

There are many thousands of commands in Emacs, so not all of them have
keyboard shortcuts. You can run an arbitrary command by pressing
`M-x`, typing in the name, and pressing `RET`. (Actually, you only
have type in enough for Emacs to filter the list down to the one you
want. Emacs also remembers which commands you use frequently, and
suggests those first.)

So, if the `transpose-frame` command doesn't have a keyboard shortcut,
we'll refer to it as `M-x transpose-frame`.

## Providing arguments to commands

Emacs has a notation called a *prefix argument*. The idea is that you
can press some keys before running a command in order to modify the
behavior of the command.

There are basically four ways to provide a prefix argument:

* Pressing `C-u` and then typing a number. The prefix argument is then
  the number you typed in.
* Typing in a number while holding Meta. (Actually, you only need to
  hold Meta while you are typing the first digit.) The prefix argument
  is again the number you typed in.
* Pressing `C-u` some number of times without typing a number. One
  `C-u` is a prefix argument of 4, two is a prefix argument of 16,
  three is 64, and so on.
* Pressing `M--` (this means the minus key with Meta held down). This
  means the prefix argument is −1.

Some commands interpret the prefix argument as a repetition count. For
instance, `C-u 100 d` will insert 100 copies of the letter "d"; on the
other hand, `C-u 100 C-d` will delete the next 100 characters.

Other commands are meant to be used with repeated `C-u`'s. With these,
one `C-u` might cause the behavior of the command to change, and a
second might cause it to change again.

Note that prefix arguments work both with commands triggered by
keyboard shortcuts and with commands triggered by `M-x`.

## Selecting from lists of options

Often you will need to select from one of several options. For
example, this happens when you open or save a file, request
documentation, or perform a multi-file search.

Most of these commands use a common interface. Essentially, you can
type text in order to filter the list of candidates, and then (if
necessary) navigate to the appropriate candidate before accepting it.
The sort order of the candidates and the way in which your query is
interpreted depends on the specific command, but generally Emacs tries
to "do the right thing".

For example, when you are running a command, the list is sorted first
by recency and frequency of usage, then by length. Furthermore,
filtering is done fuzzily, so that `plpnf` matches
`package-list-packages-no-fetch`.

On the other hand, when you are searching a file, matches are returned
in their order of appearance in the file, and your query is
interpreted as a regular expression, with spaces turned into `.*`
tokens. (You can match a literal space with two spaces.)

Most of the standard Emacs [navigation] and [text editing] commands
also work while selecting from a list, in the way that you would
expcet. In particular, you can use the arrow keys to select a
candidate.

[navigation]: navigation.md
[text editing]: editing.md

To accept a candidate, there are three different keys: `RET`, `C-j`,
and `TAB`. The standard one is `RET`; it simply accepts the selected
candidate.

Occasionally, however, you will want to tell Emacs to accept exactly
what you have typed, even if there is a candidate that is similar. To
do that, use `C-j` instead of `RET`. This can come up when you want to
create a buffer that has a name similar to an existing buffer. (If
there are no candidates, then `RET` acts like `C-j`; and if what you
have typed is identical to one of the candidates, then `C-j` acts like
`RET`.)

Normally, `TAB` acts like `RET`. It only has some special
functionality when you are [finding a file].

[finding a file]: find-files.md
