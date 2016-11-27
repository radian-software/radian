# Customizing Emacs

If Radian was installed correctly, you should have a directory called
`radian-local` next to the `radian` folder. Inside this directory
should be two files called `init.local.el` and `init.before.local.el`.
These are where you can customize Radian Emacs. (You can jump to them
with `M-RET e e l` and `M-RET e e b`, respectively.)

These files are evaluated when Emacs starts up, along with Radian's
`init.el`. The order is:

* `init.before.local.el`
* `init.el`
* `init.local.el`

If you make changes to these files, you will want to evaluate them
again. That can be done with `M-RET r`.

If you are concerned that you might have messed up Emacs and want to
restart it so as to throw away any weird state that might have been
built up, you can do that with `C-x M-c`.

If you additionally want to remove your packages so that fresh
versions can be automatically installed when you restart Emacs, you
can delete `~/.emacs.d/elpa` and `~/.emacs.d/quelpa`.

## How to add packages

There are lots of ways to install packages, and you will see many
different installation instructions on the Internet. Ignore all of
them; Radian already has package management figured out and you can
follow the directions below to easily add packages.

First, check if the package is hosted on [MELPA]. You can do this by
running `M-x package-list-packages` and [searching] for the package.
For example, package `neotree` is hosted on MELPA.

[melpa]: http://melpa.org
[searching]: search.md

If your package is hosted on MELPA, then all you have to do is add the
following to your `init.local.el`:

    (use-package neotree)

If your package is not hosted on MELPA, then you will have to find out
where it is hosted. Suppose for the sake of argument that `neotree`
were not hosted on MELPA. You then have to write a *formula* for the
package which tells Emacs where to get it. MELPA is actually just a
collection of these formulas, so you can run `M-x
quelpa-expand-recipe` to get the recipe for any package that is
located there. Doing that for `neotree` shows us that formula is:

    (neotree :repo "jaypei/emacs-neotree"
             :fetcher github
             :files (:defaults "icons"))

Of course, you will have to write your own formula for packages not
hosted on MELPA. There is [documentation] for that, though.

[documentation]: https://github.com/quelpa/quelpa

Once you have the formula, you can add it to your `init.local.el` as
follows:

    (use-package neotree
      :quelpa (neotree :repo "jaypei/emacs-neotree"
                       :fetcher github
                       :files (:defaults "icons")))

You can add any Elisp code for customizing the package to the
`use-package` form. For examples of that, see Radian's `init.el`
(`M-RET e e i`) and the [documentation for `use-package`].

[documentation for use-package]: https://github.com/jwiegley/use-package

## How to disable packages

Radian comes with many packages by default. If you really hate one of
them, you can add some Elisp code to your `init.before.local.el`:

    (radian-disable-package 'aggressive-indent)

Of course, this will also disable any dependencies of the package you
disabled.
