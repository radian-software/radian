# Finding files in Emacs

You can open files or create new files in Emacs by providing the
filenames as command-line arguments:

    $ emacs my-awesome-file.txt cool-project/README.md

However, you can actually open files faster from within Emacs! There
are four ways:

* *By directory.* This way is the simplest. You can use it to open any
  file on your computer, or to create a new file. You can even create
  new directories at the same time!

* *By project.* This way is faster, but only for files within a
  "project". (What is meant by "project" is explained below.) You can
  use it to quickly jump to any file in the current project, or jump
  to a file in another project. With this way, you cannot create new
  files, only open existing ones.

* *By frecency.* This way is even faster, but only for files and
  directories that you have visited recently or visit very frequently.
  (That is, files with a high "frecency".) You can only open existing
  files and directories, not create new ones.

* *By specific file.* All of Radian's dotfiles can be accessed
  instantly with keyboard shortcuts starting with `M-RET e`. For
  instance, `M-RET e e i` jumps to Emacs' `init.el`. To see all the
  shortcuts, press `M-RET e C-h`. These will not be discussed further.

If you want to do things with the filesystem that are more advanced
than opening existing files and creating new ones, then you should
instead use [Emacs' built-in filesystem manager].

[emacs' built-in filesystem manager]: dired.md

Also, if you opened one file in Emacs, then opened a different file,
the first file is still open (you do not have to find it again). Learn
more about [working with multiple files].

[working with multiple files]: buffers.md

## Finding files by directory

Press `C-x C-f` to invoke Emacs' file-finding interface. This will
display the path to the current directory, as well as all of the files
in that directory.

The current directory is the directory of the file you are currently
viewing. The directory of a [non-file buffer] is whatever the current
directory was when the buffer was created.

[non-file buffer]: buffers.md

If your cursor was on some text that looked like a filename when you
pressed `C-x C-f`, then the directory of that file will be displayed
instead, and that file will automatically be selected. This allows you
to quickly jump to a file or path that is referenced in text or a
program.

The basic usage is fairly simple. You can type in part of a filename
to filter the list, use `<up>` and `<down>` (if necessary) to select a
candidate, and then press `RET` to open that file.

If you press `RET` on a directory, then Emacs will open [its
filesystem manager] on that directory. To find a file in the
directory, press `TAB` instead of `RET`.

[its filesystem manager]: dired.md

To move up a directory, just press `DEL`. Alternatively, you can
select the `..` directory.

To create a new file, you can normally just type in the filename and
press `RET`. However, suppose that your new file is called "foo" and
there is already a file called "foobar" in the same directory. Emacs
will think you want to open "foobar"! To prevent this, use `C-j`
instead of `RET`.

Many of Emacs' [text navigation commands] also work in the
file-finding interface. Particularly useful are:

| Command | Normal usage                   | Usage in `C-x C-f`         |
| ------- | ------------------------------ | -------------------------- |
| `C-a`   | jump to beginning of line      | jump to beginning of entry |
| `C-e`   | jump to end of line            | jump to end of entry       |
| `C-k`   | kill rest of line              | kill rest of entry         |
| `M-<`   | jump to beginning of buffer    | jump to first candidate    |
| `M->`   | jump to end of buffer          | jump to last candidate     |

[text navigation commands]: navigation.md

You can also type `~` to jump to your home directory, and `//` to jump
to the root of your filesystem.

## Finding files by project

Emacs is pretty smart about figuring out what a project is. For
example, Git, Mercurial, Bazaar, and Leiningen projects are all
recognized, among others. If you want to mark another directory as a
project, just place an empty `.projectile` file in it.

Press `C-c p f` to display a list of files in the current project. You
can narrow this list by typing, just like when you are finding a file
as described in the previous section.

You can instead use `C-c p d` to display a list of directories in the
current project, or `C-c p b` to show buffers associated with the
current project.

To find a file in another project, press `C-c p p` and you will be
presented with a list of all the projects Emacs has seen so far. Once
you select one, you will be presented with a list of all the files and
buffers associated with the project.

There is also an "all-in-one" operation that you can trigger with
`C-c p SPC`. If you are not in a project, then this will act like
`C-c p p`. Otherwise, it will act like a combination of `C-c p f` and
`C-c p b`, showing you both files and buffers. In the latter case, you
can subsequently press `M-SPC` to switch to `C-c p p`.

## Finding files by frecency

Just press `C-c f`, and you will be presented with a list of
frequently and recently visited files and directories, sorted by
"frecency". (This functionality is provided by [fasd].) Type to filter
the list and press `RET` to accept an option.

[fasd]: https://github.com/raxod502/fasd
