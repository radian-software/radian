# Style guidelines

* Comment verbosely.
* Commit verbosely.

## Emacs Lisp

### Functions

* When referring to functions as symbols, use sharp quotes. This means
  `#'ignore` instead of `'ignore`.
* Put a docstring on every function and variable, even the internal
  ones.
* Prefer `with-eval-after-load` to `eval-after-load`.

### Advice

* Name all advice functions as `radian--advice-some-descriptive-name`.
  If you need to add `ignore` as an `:override` advice on something,
  use

    (defalias 'radian--advice-descriptive-name #'ignore)

  This means that only Radian functions should be added as advice
  functions. Never add a lambda as an advice function.

### Hooks

* Only add named functions to hooks. Never add a lambda to a hook.

### use-package

* Don't use `:ensure` or `:defer`. These are on by default in Radian.
  To eagerly load a package, use `:demand`.
* Don't use `:delight`; prefer `:diminish`.
* Don't use `:pin`; this is not relevant since Radian uses
  `straight.el` instead of `package.el`.
* If you are using `:demand`, use `:config` instead of `:init`.
* Load
* Order `use-package` keywords as follows, so that they are in the
  rough order of execution:
    * `:recipe`
    * `:defer-install`
    * `:commands`
    * `:mode`
    * `:interpreter`
    * `:init`
    * `:bind`
    * `:bind*`
    * `:bind-keymap`
    * `:bind-keymap*`
    * `:diminish`
    * `:demand`
    * `:after`
    * `:config`
