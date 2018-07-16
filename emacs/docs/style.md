* When in doubt, add more comments.
* When in doubt, explain more in the commit message.
* When referring to functions as symbols, use sharp quotes. This means
  `#'ignore` instead of `'ignore`.
* Put a docstring on every function and variable, even the internal
  ones.
* Use `use-feature` instead of `with-eval-after-load`.
* Use `radian-defadvice` and `radian-defhook` where possible, but use
  `advice-add` and `add-hook` otherwise.
* Unless a function or variable is considered part of the API exposed
  to local Radian configurations, it should be internal:
  `radian--some-name`.
* Name advices as `radian--advice-some-name`. Never use lambdas as
  advices.
* Never use lambdas as hooks.
* Follow the existing examples for setting
  `flycheck-disabled-checkers` and `company-backends`.
* Never use `:defer` in `use-package`; this is the default. To load
  eagerly, use `:demand`.
* Don't use `:ensure`, `:diminish`, `:pin`.
* Instead of `:straight nil`, prefer replacing `use-package` with
  `use-feature`.
* Use `:config` and not `:init` with `:demand`.
* Load packages lazily (this is the default) unless this is impossible
  or silly. For partial lazy-loading, use `el-patch`.
* Order `use-package` keywords as follows, so that they are in the
  rough order of execution:
  * `:preface`
  * `:straight`
  * `:commands`
  * `:init/el-patch`
  * `:init`
  * `:mode`
  * `:interpreter`
  * `:hook`
  * `:bind`
  * `:bind*`
  * `:bind-keymap`
  * `:bind-keymap*`
  * `:demand`
  * `:after`
  * `:config/el-patch`
  * `:config`
  * `:delight`
* Don't use `defcustom` unless a lot of people really are going to
  want to change the value.
* When the need for additional customization arises, consider adding
  another hook that can be run by `radian--run-hook`.
* Use two semicolons for comments. Three or more are used for section
  headers. Avoid end-of-line comments in most situations.
* Maximum line length is 79 characters.
* When enabling a minor mode, use `+1` as the argument. When
  disabling, use `-1`.
* Prefer remapping bindings instead of establishing new ones for the
  same keys.
* Prefer `:init/el-patch` and `:config/el-patch` instead of manual
  invocations of `el-patch` functions.
* Always update docstrings of `el-patch`-patched functions using
  `el-patch-concat`.
* Most code relating to a specific feature should be put inside a
  `use-package` or `use-feature` form. If the feature name differs
  from the package, then use a separate `straight-use-package` form
  combined with `use-feature`. Each of these top-level forms should
  have a comment explaining the point of the feature, starting with
  ``Feature `<name>' ...`` or ``Package `<name>' ...`` or ``Feature
  `<name>' from package `<name>'``.
* Link to relevant bug reports, pull requests, online sources, etc. in
  comments and docstrings.
* Suppress all possible superfluous messages.
* Leave keybindings as vanilla as possible.
* Use default Emacs indentation. No tabs, please.
* For enabling functionality or modes, follow the naming conventions
  of existing functions with `enable` or `setup` in their names.
* For packages which provide Company backends or Flycheck checkers,
  follow the conventions already in place.
* Consider adding a new section when you have a lot of code that goes
  together nicely.
* For each of the language sections, include a link to either the
  official website or some standard reference like Wikipedia (if no
  official website exists).
