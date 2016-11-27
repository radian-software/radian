# Emacs

Emacs is a text editor with many advanced capabilities. Currently, it
is the most capable and most customizable text editor for many (but
not all) tasks.

The next few sections give a brief introduction of how to use Emacs as
a basic text editor. At [the bottom] there are some links to
information about more advanced features.

[the bottom]: #learn-more

## How to start Emacs

Start emacs from the command line as follows:

    $ emacs

If you want to use Emacs in a standalone window, instead of in the
terminal, you can do:

    $ emacsw

## How to open a file in Emacs

Press `<ctrl+x>` and then `<ctrl+f>`. You will see the path of the
current directory displayed at the bottom of the screen. Type in part
of a filename to filter the list of files, select a file using the
arrow keys, and press `<return>` to open the selected file.

To go down a directory, select the directory and press `<tab>`. To go
up, just press `<delete>`, or select the `..` directory and press
`<return>`. Or, if you change your mind, press `<ctrl+g>` to cancel
opening a file.

If you need to create a file that has a name similar to an
already-existing file or directory, you can press `<ctrl+j>` to tell
Emacs to use your name instead of opening an existing file.

## How to edit a file in Emacs

There are lots of editing commands, but you can still use the arrow
keys to move the cursor and type to enter text, just like in any other
text editor. You can also move the cursor by clicking the mouse.

## How to save a file in Emacs

Press `<ctrl+x>` and then `<ctrl+s>` to save. You can use `<ctrl+x>`
and then `<ctrl+w>` for "save as", in which case you will need to
select a new filename and/or directory. The interface for this is just
like the one for opening a file.

## How to cancel things

Press `<ctrl+g>`.

## How to exit Emacs

Press `<ctrl+x>` and then `<ctrl+c>`. If you have files with unsaved
changes, you will be prompted to save them.

## Learn more

First, you should probably read about [Emacs fundamentals] to learn
about the ideas that are used everywhere in Emacs.

[emacs fundamentals]: fundamentals.md

Then, pick your favorite topic:

* [Opening and creating files](find-files.md)
* [Save files](save-files.md)
* [Navigation within a file](navigation.md)
* [Basic text editing](editing.md)
* [Searching, find and replace](search.md)
* [Editing more than one file at once](buffers.md)
* [Undo/redo](undo-redo.md)
* [Autocompletion](completion.md)
* [Renaming files, creating directories, etc.](dired.md)
* [Programming](programming.md)
* [Getting help](help.md)
* [Customizing Emacs](customizing.md)
