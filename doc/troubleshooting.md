# Troubleshooting

This file collects some useful tips for debugging when there are
issues with Radian.

## Failed to byte-compile: Debugger entered

Radian byte-compiles the init-file asynchronously after a successful
init. If there is an error, the first line of the error message is
printed in the minibuffer. You can get the whole error message by
running `make compile` in the Radian repository, which is the same
thing Radian is doing in the background.
