# Windows and buffers in Emacs

Emacs has very precise terminology to refer to the different
components of its interface that you use to edit files.

* *Frame.* What most programs call a window, Emacs calls a frame. This
  is because there is a lot going on inside Emacs, and the literal
  boundaries of the Emacs' user interface serve as a frame to hold it
  all in. If you are using Emacs in the terminal, you can still have
  multiple frames, but only one of them can be displayed at a time.
  Many people always use a single frame to do all their work in Emacs.

* *Window.* Inside each frame, there can be multiple windows, each of
  which can display something different. The existence of windows is
  why frames are not as popular in Emacs as they are in other editors.
  Windows are very easy to manipulate precisely, so clicking and
  dragging is rarely necessary.

* *Buffer.* A buffer is Emacs' conception of a file-like object. The
  difference is that in Emacs, buffers do not have to correspond to
  files—there are help buffers, filesystem navigation buffers,
  customization buffers, and so on. Each window in a frame shows part
  of a buffer, although multiple windows can show different parts of
  the same buffer simultaneously. All of them show the same buffer, so
  if you type in one window, your changes will appear instantly on all
  other windows showing the same buffer.

It's rarely necessary to deal with multiple frames, so that is not
covered in this documentation.

## Moving between windows

To move from one window to another, simply hold Shift and press the
direction of the window you want to move to. There is only one
interesting thing to say about this, which is what happens when there
is more than one window that could be moved to. For instance, what if
you are in window A and press `S-<right>`?

    +-------+-------+
    |       |       |
    |       |   B   |
    |       |       |
    |   A   +-------+
    |       |       |
    |       |   C   |
    |       |       |
    +-------+-------+

The answer is that if your cursor is in the top half of A, then you
will move to B, whereas if you are in the bottom half of A, then you
will move to C.

## Creating, killing, and resizing windows

Note that this is a different subject than creating and killing
buffers, since windows are just views into buffers, and typically most
of your buffers will not be shown in windows.

To split the current window horizontally, use `C-x 2`. To split it
vertically instead, use `C-x 3`.

To kill the current window, use `C-x 0`. This will only work if there
is more than one window, of course. To kill all windows except the
current one, use `C-x 1`.

Some buffers, like the help and customization buffers (and many
others) are considered *transient*, and you can close their windows
just by pressing `q`.

To resize a window, you can just click and drag on the window
boundary.

## Changing which buffer a window shows

You basically have three options:

* Switch to an existing buffer by pressing `C-x b`, typing in part of
  the name of a buffer, and selecting your choice with `RET`.
* Create a new, empty buffer by pressing `C-x b` and typing in its
  name. If the name is similar to an existing buffer, you will have to
  use `C-j` instead of `RET` to convince Emacs to create a new buffer
  instead of switching to the existing buffer.
* Create a new buffer with the content of a file by [navigating to the
  file].

[navigating to the file]: files.md

## Killing buffers

Use `C-x k` to kill a buffer. Please note that this is different from
killing a window (`C-x 0`), which does not affect the window's buffer.

## Power tools for managing your window configurations

You can make the current window show whatever buffer it was previously
showing using `C-x <left>`, and you can undo the effects of that
command using `C-x <right>`.

You can also "undo" through entire previous window configurations by
pressing `C-c <left>` repeatedly. If you change your mind, you can
press `C-c <right>` immediately after all of your undos to get back to
your original state. If you've already done another command, then you
can no longer use `C-c <right>`, but it's still possible to get back
to where you were using another `C-c <left>`. This is because your
sequence of undos is actually treated as another change that can be
undone.

The following functions can be used to rearrange windows in the
current frame:

    M-x transpose-frame  ...  Swap x-direction and y-direction

           +------------+------------+      +----------------+--------+
           |            |     B      |      |        A       |        |
           |     A      +------------+      |                |        |
           |            |     C      |  =>  +--------+-------+   D    |
           +------------+------------+      |   B    |   C   |        |
           |            D            |      |        |       |        |
           +-------------------------+      +--------+-------+--------+

    M-x flip-frame  ...  Flip vertically

           +------------+------------+      +------------+------------+
           |            |     B      |      |            D            |
           |     A      +------------+      +------------+------------+
           |            |     C      |  =>  |            |     C      |
           +------------+------------+      |     A      +------------+
           |            D            |      |            |     B      |
           +-------------------------+      +------------+------------+

    M-x flop-frame  ...  Flop horizontally

           +------------+------------+      +------------+------------+
           |            |     B      |      |     B      |            |
           |     A      +------------+      +------------+     A      |
           |            |     C      |  =>  |     C      |            |
           +------------+------------+      +------------+------------+
           |            D            |      |            D            |
           +-------------------------+      +-------------------------+

    M-x rotate-frame  ...  Rotate 180 degrees

           +------------+------------+      +-------------------------+
           |            |     B      |      |            D            |
           |     A      +------------+      +------------+------------+
           |            |     C      |  =>  |     C      |            |
           +------------+------------+      +------------+     A      |
           |            D            |      |     B      |            |
           +-------------------------+      +------------+------------+

    M-x rotate-frame-clockwise  ...  Rotate 90 degrees clockwise

           +------------+------------+      +-------+-----------------+
           |            |     B      |      |       |        A        |
           |     A      +------------+      |       |                 |
           |            |     C      |  =>  |   D   +--------+--------+
           +------------+------------+      |       |   B    |   C    |
           |            D            |      |       |        |        |
           +-------------------------+      +-------+--------+--------+

    M-x rotate-frame-anticlockwise  ...  Rotate 90 degrees anti-clockwise

           +------------+------------+      +--------+--------+-------+
           |            |     B      |      |   B    |   C    |       |
           |     A      +------------+      |        |        |       |
           |            |     C      |  =>  +--------+--------+   D   |
           +------------+------------+      |        A        |       |
           |            D            |      |                 |       |
           +-------------------------+      +-----------------+-------+

Finally, when none of the above functions work for you, you can swap
the buffer in the current window with the buffer in an adjacent window
using `M-x buf-move-<direction>`, where `<direction>` is either
`left`, `right`, `up`, or `down`.
