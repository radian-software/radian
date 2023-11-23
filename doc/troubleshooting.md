# Troubleshooting

This file collects some useful tips for debugging when there are
issues with Radian.

## Failed to byte-compile: Debugger entered

Radian byte-compiles the init-file asynchronously after a successful
init. If there is an error, the first line of the error message is
printed in the minibuffer. You can get the whole error message by
running `make compile` in the Radian repository, which is the same
thing Radian is doing in the background.

## Things break elsewhere in the system when loading Radian Emacs

By default Radian will source `~/.profile` asynchronously after Emacs
startup to ensure that environment variables are set correctly when
Emacs is started from a desktop environment that doesn't configure
this properly. This is like
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell),
but way faster.

If your `~/.profile` is not safe to source multiple times, consider
having it set an environment variable that will cause future
invocations to do nothing. Alternatively, add `(setq radian-env-setup
nil)` to `~/.emacs.d/init.local.el` to disable this feature.
