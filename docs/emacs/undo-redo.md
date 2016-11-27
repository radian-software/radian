# Undo/redo

The problem with most editors is that if you undo some changes and
then make an edit, you have lost all the changes you just undid. In
Emacs, those changes are not gone—they just form another branch of the
*undo tree*.

The basic usage is quite simple. Just press `C-/` to undo and `M-/` to
redo. But if you need to visualize the underlying undo tree, you can
use `C-x u`. From there, just navigate around using the arrow keys and
dismiss the visualization buffer with `q` when you have found the
version you are looking for.
