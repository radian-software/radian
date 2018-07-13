The comments in Radian are extensive. However, there are some common
design patterns and concepts that would be repetitive to document
repeatedly, and these are documented here.

## `use-package`

This is a macro that basically wraps `with-eval-after-load`. Code in
`:config` is run after the named feature is loaded, while code in
`:init` is run immediately. However, `use-package` does many more
things.

For example, `use-package` interfaces with Radian's package manager,
`straight.el`. By default, `use-package` will use `straight.el` to
install a package with the same name as the feature, before running
its `:init` code. The package name can be overridden by passing a
symbol to `:straight`, and a custom recipe can be specified there as
well. The curious developer should refer to the (extensive)
documentation of `straight.el` on recipe format.

If `:straight nil` is given, then `use-package` does not attempt to
install any package, and instead acts just as `with-eval-after-load`
(with many more features).

The `:after` keyword causes the given feature to be loaded
automatically after the named one. This keyword has some interesting
additional options beyond just a symbol, which is currently
undocumented upstream.

The `:bind`, `:bind*`, `:bind-keymap`, and `:bind-keymap*` keywords
delegate to the `bind-key` package. They establish keybindings in
various interesting ways.

The `:mode` and `:interpreter` keywords append to `auto-mode-alist`
and `interpreter-mode-alist`, respectively.

Keyword `:demand` causes the feature to be required immediately. The
only advantage of using `use-package` in this case is all the
syntactic sugar, which is a nontrivial advantage. Keyword `:defer` is
superfluous and has no effect in Radian's configuration, unless an
integer argument is provided in which case it causes the feature to be
required after that many seconds after init.

Keyword `:diminish` interfaces with the `:diminish` package, which is
used to remove and rename mode line indicators for minor modes (but
not major modes).

Keyword `:pin` is superfluous and has no effect in Radian's
configuration.

## `el-patch`

We use the `:init/el-patch` and `:config/el-patch` keywords in
`use-package` when possible. Otherwise we make sure to use
`el-patch-feature`, `el-patch-pre-validate-hook`, and
`el-patch-post-validate-hook` appropriately.

## Common modes

We enable `eldoc-mode` explicitly in all modes where it has an effect,
since if you enable `eldoc-mode` globally then it will print a warning
when a buffer does not support it.

## Keybindings

`RET` is the keybinding for return in terminal Emacs, which is
equivalent to `C-i`; `<return>` is the same for windowed Emacs.
